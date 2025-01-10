{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Bluesky where

import Control.Monad
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import GHC.Generics

import qualified Bluesky.Did as Did
import qualified Bluesky.Handle as Handle
import qualified Crypto.JOSE.JWK as JWK
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Network.URI.Static as URI
import qualified Servant

import qualified Api
import qualified ClientAssertion
import qualified DPoP
import qualified MakeJwt
import qualified PKCE
import qualified Sessions

data Session = Session
  { state :: BS.ByteString
  , dpopJwk :: JWK.JWK
  , dpopNonce :: Text
  , pkce :: PKCE.PKCE
  , did :: Did.Did
  , authorizationServer :: URI.URI
  , tokenURI :: URI.URI
  }

data Env = Env
  { httpManager :: HTTP.Manager
  , clientAssertion :: ClientAssertion.Env
  , sessions :: Sessions.Store Session
  , jwt :: MakeJwt.Env
  }

clientId :: URI.URI
clientId = [URI.uri|https://reciprocity.rpm.cc/auth/bluesky/client_metadata.json|]

init :: HTTP.Manager -> MakeJwt.Env -> IO Env
init httpManager jwt = do
  clientAssertion <- ClientAssertion.init clientId
  sessions <- Sessions.newStore
  pure Env{ httpManager, clientAssertion, sessions, jwt }

newtype AesonURI = AesonURI URI.URI
  deriving stock (Eq, Ord, Show)

instance Aeson.FromJSON AesonURI where
  parseJSON = Aeson.withText "AesonURI" $ \t ->
    maybe (fail $ "Couldn't parse URI: " <> show t) (pure . AesonURI)
      $ URI.parseURI (Text.unpack t)

instance Aeson.ToJSON AesonURI where
  toJSON (AesonURI uri) = Aeson.String (Text.pack (URI.uriToString id uri ""))

-- | https://atproto.com/specs/oauth#clients
clientMetadata :: Env -> Text -> IO Aeson.Value
clientMetadata Env{ clientAssertion } host = do
  jwks <- ClientAssertion.getPublicKeys clientAssertion
  pure $ Aeson.object
    [ "client_id" .= AesonURI clientId
    , "application_type" .= ("web" :: Text)
    , "grant_types" .= ["authorization_code" :: Text]
    , "scope" .= ("atproto" :: Text)
    , "response_types" .= ["code" :: Text]
    , "redirect_uris" .= [base_uri <> "/login/bluesky/complete"]
    , "token_endpoint_auth_method" .= ("private_key_jwt" :: Text)
    , "token_endpoint_auth_signing_alg" .= ("ES256" :: Text)
    , "dpop_bound_access_tokens" .= True
    , "jwks" .= jwks
    , "client_name" .= ("flexiprocity" :: Text)
    , "client_uri" .= ("https://" <> host)
    ]
  where
    base_uri = "https://" <> host <> "/auth"

serveClientMetadata :: Env -> Maybe Text -> Servant.Handler Aeson.Value
serveClientMetadata _ Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
serveClientMetadata env (Just host) =
  liftIO $ clientMetadata env host

data AuthServerInfo = AuthServerInfo
  { issuer :: AesonURI -- doc says this is relevant, idk why yet
  , pushed_authorization_request_endpoint :: AesonURI
  , authorization_endpoint :: AesonURI
  , token_endpoint :: AesonURI
  , scopes_supported :: [Text] -- should include "atproto"
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON)

or400 :: BSL.ByteString -> Maybe a -> Servant.Handler a
or400 errBody =
  maybe
    (Except.throwError Servant.err400{ Servant.errBody })
    pure

start :: Env -> Maybe Handle.Handle -> Servant.Handler Api.CookieRedirect
start Env{ httpManager, clientAssertion, sessions } mHandle = do
  handle <- or400 "Query parameter handle is required" mHandle
  did <- maybe (fail "Can't find DID") pure
    =<< liftIO (Handle.resolveVerify httpManager handle)
  doc <- maybe (fail "Can't get DID document") pure
    =<< liftIO (Did.getDocument httpManager did)
  pds <- maybe (fail "Can't find pds") pure
    $ Did.getPds doc
  authorizationServer <- liftIO $ do
    req <- HTTP.requestFromURI pds{ URI.uriPath = "/.well-known/oauth-protected-resource" }
    resp <- HTTP.httpLbs req httpManager
    obj <- maybe (fail "Couldn't decode OPR response") pure
      $ Aeson.decode (HTTP.responseBody resp)
    serverStrings <- either fail pure
      $ Aeson.parseEither (.: "authorization_servers") obj
    serverURIs <- forM serverStrings $ \s ->
      maybe (fail "Couldn't parse authorization_servers URI") pure
      $ URI.parseURI s
    case serverURIs of
      [] -> fail "No authorization_servers listed"
      uri : _ ->
        -- maybe pick one at random or something, but IME there's only one
        pure uri
  AuthServerInfo
    { pushed_authorization_request_endpoint = AesonURI parURI
    , authorization_endpoint = AesonURI authURI
    , token_endpoint = AesonURI tokenURI
    } <- liftIO $ do
    req <- HTTP.requestFromURI
      authorizationServer{ URI.uriPath = "/.well-known/oauth-authorization-server" }
    resp <- HTTP.httpLbs req httpManager
    either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp
  pkce@PKCE.PKCE{ challenge } <- liftIO PKCE.makePKCE
  state <- BSC.pack <$> liftIO Sessions.randomString
  dpopJwk <- liftIO DPoP.createJwk
  dpopNonce <- liftIO $ do
    -- First request deliberately failed in order to get a DPoP nonce
    getDpopReq <- HTTP.requestFromURI parURI
      <&> HTTP.urlEncodedBody []
    dpopResp <- HTTP.httpLbs getDpopReq httpManager
    case lookup "DPoP-Nonce" $ HTTP.responseHeaders dpopResp of
      Nothing -> fail "No DPoP-Nonce"
      Just n -> pure (Text.decodeASCII n)
  requestUri <- liftIO $ do
    authJwt <- ClientAssertion.makeAssertion clientAssertion authorizationServer
    req <- HTTP.requestFromURI parURI
      <&> HTTP.urlEncodedBody
        [ ("client_id", BSC.pack $ URI.uriToString id clientId "")
        , ("response_type", "code")
        , ("code_challenge", challenge)
        , ("code_challenge_method", "S256")
        , ("state", state)
        , -- Sadly this has to be under the same origin as the client_id, which
          -- has to be a publicly accessible URL. So I don't see a way to
          -- support local dev here.
          ("redirect_uri", "https://reciprocity.rpm.cc/auth/login/bluesky/complete")
        , ("scope", "atproto")
        , ("login_hint", Text.encodeUtf8 (Handle.rawHandle handle))
        , ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
        , ("client_assertion", authJwt)
        ]
      >>= DPoP.dpopRequest (Just dpopNonce) dpopJwk
    resp <- HTTP.httpLbs req httpManager
    obj <- maybe (fail "Couldn't decode PAR response") pure
      $ Aeson.decode (HTTP.responseBody resp)
    either fail pure $ Aeson.parseEither (.: "request_uri") obj
  let
    uriQuery =
      -- maybe I should be percent-encoding these parameters but idk seems to work fine
      "?request_uri=" <> Text.unpack requestUri
      <> "&client_id=" <> URI.uriToString id clientId ""
    session =
      Session { state, dpopJwk, dpopNonce, pkce, did, authorizationServer, tokenURI }
  sessId <- liftIO Sessions.newSessionId
  liftIO $ Sessions.setSession sessId session sessions
  pure
    $ Servant.addHeader (Sessions.sessionIdCookie "/auth/login/bluesky" sessId)
    $ Servant.addHeader (Api.Location authURI{ URI.uriQuery })
    $ Servant.NoContent

data TokenResponse = TokenResponse
  { access_token :: Text
  , scope :: Text
  , sub :: Did.Did
  } deriving stock (Generic)
    deriving anyclass (Aeson.FromJSON)

-- To be honest, I don't fully understand why authentication is not already
-- complete at this point. The client is sending us the state token, which we
-- only sent to the auth server, and I think the auth server only sends it to
-- the client if they completed the auth. But the docs explicitly say when
-- authentication clients can stop, and it's not yet. We have to do more
-- verification first.
complete
  :: Env
  -> Maybe Sessions.SessionId
  -> Maybe Api.Issuer -> Maybe Text -> Maybe Text
  -> Servant.Handler Api.CookieRedirect
complete Env{ clientAssertion, httpManager, sessions, jwt }
    mSessId mIss mState mCode = do
  sessId <- or400 "No session ID" mSessId
  Api.Issuer iss <- or400 "Expected query parameter: iss" mIss
  stateFromClient <- or400 "Expected query parameter: state" mState
  code <- or400 "Expected query parameter: code" mCode
  Session{ state, dpopJwk, dpopNonce, pkce, did, authorizationServer, tokenURI }
    <- or400 "Can't find your session. Probably it just expired."
      =<< liftIO (Sessions.getSession sessId sessions)
  when (iss /= authorizationServer) $
    Except.throwError Servant.err400
      { Servant.errBody = "iss query parameter doesn't match auth server" }
  when (stateFromClient /= Text.decodeUtf8 state) $
    Except.throwError Servant.err400
      { Servant.errBody = "state query parameter doesn't match session" }
  TokenResponse{ scope, sub } <- liftIO $ do
    authJwt <- ClientAssertion.makeAssertion clientAssertion authorizationServer
    req <- HTTP.requestFromURI tokenURI
      <&> HTTP.urlEncodedBody
        [ ("grant_type", "authorization_code")
        , ("code", Text.encodeUtf8 code)
        , ("redirect_uri", "https://reciprocity.rpm.cc/auth/login/bluesky/complete")
        , ("code_verifier", PKCE.verifier pkce)
        , ("client_id", BSC.pack $ URI.uriToString id clientId "")
        , ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
        , ("client_assertion", authJwt)
        ]
      >>= DPoP.dpopRequest (Just dpopNonce) dpopJwk
    resp <- HTTP.httpLbs req httpManager
    either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp
  when (scope /= "atproto") $
    Except.throwError Servant.err400
      { Servant.errBody = "unexpected scope" }
  when (sub /= did) $
    Except.throwError Servant.err400
      { Servant.errBody = "token does not belong to user" }
  cookie <- liftIO
    $ MakeJwt.cookie jwt
    $ Map.singleton "bluesky" (Aeson.String $ Did.rawDid did)
  pure
    $ Servant.addHeader (Text.decodeUtf8 cookie)
    $ Servant.addHeader (Api.Location [URI.relativeReference|/|])
    $ Servant.NoContent
