{-# LANGUAGE QuasiQuotes #-}
module Friendica where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Control.Monad
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable
import Data.Functor
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Network.URI.Static as URI
import qualified Servant

import qualified Api
import qualified MakeJwt
import qualified Secrets
import qualified Sessions

data Instance = Instance
  { clientId :: Text
  , clientSecret :: Text
  , baseUrl :: URI.URI
  }

instance Aeson.FromJSON Instance where
  parseJSON = Aeson.withObject "Instance" $ \o -> do
    clientId <- o .: "client_id"
    clientSecret <- o .: "client_secret"
    baseUrlText <- o .: "base_url"
    Just baseUrl <- pure $ URI.parseAbsoluteURI (Text.unpack baseUrlText)
    pure Instance{ clientId, clientSecret, baseUrl }

data Env = Env
  { sessions :: Sessions.Store
  , httpManager :: HTTP.Manager
  , jwt :: MakeJwt.Env
  , instances :: Map.Map Api.InstanceName Instance
  }

init :: Sessions.Store -> HTTP.Manager -> MakeJwt.Env -> IO Env
init sessions httpManager jwt = do
  instances <- Secrets.getJson "friendica_instances.json"
  pure Env{ sessions, httpManager, jwt, instances }

orError :: (Except.MonadError e m) => e -> Maybe a -> m a
orError err = maybe (Except.throwError err) pure

start :: Env -> Api.InstanceName -> Maybe Text -> Servant.Handler Api.CookieRedirect
start _ _ Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
start Env{ sessions, instances } instanceName (Just host) = do
  Instance{ clientId, clientSecret = _, baseUrl }
    <- orError Servant.err404 $ Map.lookup instanceName instances
  sessId <- liftIO $ Sessions.newSessionId
  state <- liftIO $ BSC.pack <$> Sessions.randomString
  let
    redirectUriString = concat
      [ "https://"
      , Text.unpack host
      , "/auth/login/friendica/complete/"
      , Text.unpack (Api.getInstanceName instanceName)
      ]
    friendicaUriQuery =
      concat
        [ "?client_id=" <> Text.unpack clientId
        , "&redirect_uri="
          <> URI.escapeURIString URI.isUnreserved redirectUriString
        , "&response_type=code"
        , "&state="
          <> URI.escapeURIString URI.isUnreserved (BSC.unpack state)
        ]
    friendicaUri =
      baseUrl
        { URI.uriQuery = friendicaUriQuery
        , URI.uriPath = "/oauth/authorize"
        }
    redirectUri = BSC.pack redirectUriString
  -- OAuth 1 doesn't use nonce, I think
  liftIO $ Sessions.setSession sessId Sessions.Session{ state, nonce = "", redirectUri } sessions
  pure
    $ Servant.addHeader (Sessions.sessionIdCookie sessId)
    $ Servant.addHeader (Api.Location friendicaUri)
    $ Servant.NoContent

data Token = Token
  { access_token :: Text
  , me :: Text
  -- ignored: token_type, scope, created_at
  } deriving stock (Generic, Show)
    deriving anyclass (Aeson.FromJSON)

data VerifyCredentials = VerifyCredentials
  { id_str :: Text }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON)

complete
  :: Env
  -> Api.InstanceName
  -> Maybe Sessions.SessionId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Servant.Handler Api.CookieRedirect
complete
    Env{ httpManager, sessions, jwt, instances } instanceName
    maybeSessId maybeError maybeCode maybeState
  = do
  Instance{ clientId, clientSecret, baseUrl } <-
    orError Servant.err404 $ Map.lookup instanceName instances
  let
    or400 msg = orError Servant.err400{ Servant.errBody = msg }
  sessId <- or400 "no session id" maybeSessId
  Sessions.Session{ state, nonce = _, redirectUri }
    <- or400 "unknown session id" =<< liftIO (Sessions.getSession sessId sessions)
  for_ maybeError $ \msg ->
    Except.throwError Servant.err400{
      Servant.errBody = "Error from friendica: " <> TLE.encodeUtf8 (TL.pack (show msg))
    }
  code <- or400 "No code, but no error either?" maybeCode
  clientState <- or400 "No state" maybeState
  liftIO $ Sessions.deleteSession sessId sessions
  when (state /= Text.encodeUtf8 clientState)
    $ Except.throwError Servant.err400{ Servant.errBody = "state mismatch" }
  t@Token{ access_token } <- do
    req <-
      HTTP.parseUrlThrow
        ("POST " <> URI.uriToString id baseUrl "/oauth/token")
      <&> HTTP.urlEncodedBody
        [ ("client_id", Text.encodeUtf8 clientId)
        , ("client_secret", Text.encodeUtf8 clientSecret)
        , ("redirect_uri", redirectUri)
        , ("code", Text.encodeUtf8 code)
        , ("grant_type", "authorization_code")
        ]
    resp <- liftIO $ HTTP.httpLbs req httpManager
    either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp
  liftIO $ print t
  let
    withAuth req =
      req
        { HTTP.requestHeaders
            = ( "Authorization"
              , "Bearer " <> Text.encodeUtf8 access_token
              ) : HTTP.requestHeaders req
        }
  VerifyCredentials{ id_str } <- do
    req <-
      withAuth <$> HTTP.parseUrlThrow
        ("GET " <> URI.uriToString id baseUrl "/api/account/verify_credentials")
    resp <- liftIO $ HTTP.httpLbs req httpManager
    either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp
  cookie <- liftIO $ MakeJwt.cookie jwt
    $ Map.singleton "friendica"
    $ Aeson.toJSON
    $ Map.singleton instanceName id_str
  pure
    $ Servant.addHeader (Text.decodeUtf8 cookie)
    $ Servant.addHeader (Api.Location [URI.relativeReference|/|])
    $ Servant.NoContent
