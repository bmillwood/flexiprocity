{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Bluesky where

import Control.Monad
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.Functor
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import GHC.Generics

import qualified Bluesky.Did as Did
import qualified Bluesky.Handle as Handle
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Network.URI.Static as URI
import qualified Servant

import qualified Api
import qualified ClientAssertion
import qualified DPoP
import qualified PKCE
import qualified Sessions

data Env = Env
  { httpManager :: HTTP.Manager
  , clientAssertion :: ClientAssertion.Env
  }

clientId :: URI.URI
clientId = [URI.uri|https://reciprocity.rpm.cc/auth/bluesky/client_metadata.json|]

init :: HTTP.Manager -> IO Env
init httpManager = do
  clientAssertion <- ClientAssertion.init clientId
  pure Env{ httpManager, clientAssertion }

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

start :: Env -> Handle.Handle -> Servant.Handler Api.CookieRedirect
start Env{ httpManager, clientAssertion } handle = do
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
    } <- liftIO $ do
    req <- HTTP.requestFromURI
      authorizationServer{ URI.uriPath = "/.well-known/oauth-authorization-server" }
    resp <- HTTP.httpLbs req httpManager
    either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp
  PKCE.PKCE{ verifier = _, challenge } <- liftIO PKCE.makePKCE
  state <-
    -- Slight abuse here, but a (fresh!) PKCE verifier is a perfectly good state
    -- token
    liftIO PKCE.makeVerifier
  requestUri <- liftIO $ do
    -- First request deliberately failed in order to get a DPoP nonce
    getDpopReq <- HTTP.requestFromURI parURI
      <&> HTTP.urlEncodedBody []
    dpopResp <- HTTP.httpLbs getDpopReq httpManager
    dpopNonce <-
      case lookup "DPoP-Nonce" $ HTTP.responseHeaders dpopResp of
        Nothing -> fail "No DPoP-Nonce"
        Just n -> pure (Text.decodeASCII n)
    dpopJwk <- DPoP.createJwk
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
  -- we don't use this sessId yet but we will need to when implementing /complete
  sessId <- liftIO Sessions.newSessionId
  pure
    $ Servant.addHeader (Sessions.sessionIdCookie sessId)
    $ Servant.addHeader
        (Api.Location authURI{ URI.uriQuery })
    $ Servant.NoContent
