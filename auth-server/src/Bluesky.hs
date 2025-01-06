{-# LANGUAGE ViewPatterns #-}
module Bluesky where

import Control.Monad
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import Data.Functor
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import GHC.Generics

import qualified Bluesky.Did as Did
import qualified Bluesky.Handle as Handle
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Servant

import qualified DPoP
import qualified PKCE

data Env = Env
  { httpManager :: HTTP.Manager }

init :: HTTP.Manager -> IO Env
init httpManager = pure $ Env{ httpManager }

-- | https://atproto.com/specs/oauth#clients
clientMetadata :: Text -> Aeson.Value
clientMetadata host = Aeson.object
  [ "client_id" .= (base_uri <> "/bluesky/client_metadata.json")
  , "application_type" .= ("web" :: Text)
  , "grant_types" .= ["authorization_code" :: Text]
  , "scope" .= ("atproto" :: Text)
  , "response_types" .= ["code" :: Text]
  , "redirect_uris" .= [base_uri <> "/login/bluesky/complete"]
  , "dpop_bound_access_tokens" .= True
  , "client_name" .= ("flexiprocity" :: Text)
  , "client_uri" .= ("https://" <> host)
  , -- this really seems like it should be the default, but the PDS insisted I
    -- specify it for some reason
    "token_endpoint_auth_method" .= ("none" :: Text)
  ]
  where
    base_uri = "https://" <> host <> "/auth"

serveClientMetadata :: Maybe Text -> Servant.Handler Aeson.Value
serveClientMetadata Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
serveClientMetadata (Just host) =
  pure (clientMetadata host)

newtype AesonURI = AesonURI URI.URI
  deriving stock (Eq, Ord, Show)

instance Aeson.FromJSON AesonURI where
  parseJSON = Aeson.withText "AesonURI" $ \t ->
    maybe (fail $ "Couldn't parse URI: " <> show t) (pure . AesonURI)
      $ URI.parseURI (Text.unpack t)

data AuthServerInfo = AuthServerInfo
  { issuer :: AesonURI -- doc says this is relevant, idk why yet
  , pushed_authorization_request_endpoint :: AesonURI
  , authorization_endpoint :: AesonURI
  , token_endpoint :: AesonURI
  , scopes_supported :: [Text] -- should include "atproto"
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON)

start :: Env -> Handle.Handle -> Servant.Handler Aeson.Value
start Env{ httpManager } handle = do
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
  liftIO $ do
    -- First request deliberately failed in order to get a DPoP nonce
    getDpopReq <- HTTP.requestFromURI parURI
      <&> HTTP.urlEncodedBody []
    dpopResp <- HTTP.httpLbs getDpopReq httpManager
    dpopNonce <-
      case lookup "DPoP-Nonce" $ HTTP.responseHeaders dpopResp of
        Nothing -> fail "No DPoP-Nonce"
        Just n -> pure (Text.decodeASCII n)
    jwk <- DPoP.createJwk
    req <- HTTP.requestFromURI parURI
      <&> HTTP.urlEncodedBody
        [ ("client_id", "https://reciprocity.rpm.cc/auth/bluesky/client_metadata.json")
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
        ]
      >>= DPoP.dpopRequest (Just dpopNonce) jwk
    resp <- HTTP.httpLbs req httpManager
    either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp
