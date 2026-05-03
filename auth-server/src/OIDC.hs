{-# LANGUAGE QuasiQuotes #-}
module OIDC where
-- https://docs.servant.dev/en/stable/cookbook/open-id-connect/OpenIdConnect.html

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP
import qualified Network.URI.Static as URI
import qualified Servant
import qualified Web.OIDC.Client as OC

import qualified Api
import qualified Diagnose
import qualified MakeJwt
import qualified Secrets
import qualified Sessions
import qualified Sentry

data Provider = Provider
  { issuer :: OC.IssuerLocation
  , clientId :: Text
  , clientSecret :: Text
  } deriving stock (Generic)

instance Aeson.FromJSON Provider where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' }

data ProviderEnv = ProviderEnv
  { providerName :: Api.ProviderName
  , sessions :: Sessions.Store Sessions.Session
  , httpManager :: HTTP.Manager
  , getProvider :: IO OC.Provider
  , provider :: Provider
  , sentry :: Sentry.Service
  }

data Env = Env
  { providers :: Map Api.ProviderName ProviderEnv
  , jwt :: MakeJwt.Env
  }

basePath :: BS.ByteString
basePath = "/auth/login/oidc"

redirectUriRelative :: Api.ProviderName -> BS.ByteString
redirectUriRelative name =
  basePath <> "/complete/" <> Text.encodeUtf8 (Api.getProviderName name)

init :: HTTP.Manager -> Sentry.Service -> MakeJwt.Env -> IO Env
init httpManager sentry jwt = do
  parsed <- Secrets.getJson "oidc_providers.json"
  providers <- Map.traverseWithKey (initProvider httpManager sentry) parsed
  pure Env{ providers, jwt }

initProvider :: HTTP.Manager -> Sentry.Service -> Api.ProviderName -> Provider -> IO ProviderEnv
initProvider httpManager sentry providerName provider@Provider{ issuer } = do
  sessions <- Sessions.newStore
  getProvider <- OC.cachedDiscover issuer httpManager
  pure ProviderEnv
    { providerName
    , sessions
    , httpManager
    , getProvider
    , provider
    , sentry
    }

lookupProvider :: Env -> Api.ProviderName -> Servant.Handler ProviderEnv
lookupProvider Env{ providers } name =
  maybe (Except.throwError Servant.err404) pure $ Map.lookup name providers

oidcWithRedirectUri :: ProviderEnv -> BS.ByteString -> IO OC.OIDC
oidcWithRedirectUri ProviderEnv{ getProvider, provider = Provider{ clientId, clientSecret } } redirectUri =
    OC.setCredentials (Text.encodeUtf8 clientId) (Text.encodeUtf8 clientSecret) redirectUri
      . OC.newOIDC <$> getProvider

start :: Env -> Api.ProviderName -> Maybe Text -> Servant.Handler Api.CookieRedirect
start _ _ Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
start env name (Just host) = do
  pe@ProviderEnv{ sessions } <- lookupProvider env name
  let redirectUri = "https://" <> Text.encodeUtf8 host <> redirectUriRelative name
  liftIO $ do
    sessId <- Sessions.newSessionId
    let
      sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
      scopes = [OC.openId, OC.profile, OC.email]
    oidc <- oidcWithRedirectUri pe redirectUri
    url <- OC.prepareAuthenticationRequestUrl sessionStore oidc scopes []
    pure
      $ Servant.addHeader (Sessions.sessionIdCookie (redirectUriRelative name) sessId)
      $ Servant.addHeader (Api.Location url)
      $ Servant.NoContent

data Claims = Claims
  { email :: Text
  , name :: Text
  , picture :: Text
  , email_verified :: Bool
  -- other keys: at_hash, au, azp, exp, family_name, given_name, iat, iss, nonce, sub
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, Show)

codeToClaims :: ProviderEnv -> Sessions.SessionId -> BS.ByteString -> BS.ByteString -> IO Claims
codeToClaims env@ProviderEnv{ sentry, sessions, httpManager } sessId code clientState = do
  Just Sessions.Session{ state = _, nonce = _, redirectUri } <- Sessions.getSession sessId sessions
  let
    sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
  oidc <- oidcWithRedirectUri env redirectUri
  OC.Tokens { idToken = OC.IdTokenClaims { otherClaims } }
    <- Sentry.reportException sentry
    $ OC.getValidTokens sessionStore oidc httpManager clientState code
  pure otherClaims

complete
  :: Env
  -> Api.ProviderName
  -> Maybe Sessions.SessionId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Servant.Handler Api.CookieRedirect
complete _ name _ (Just errMsg) _ _ = do
  liftIO . Diagnose.logMsg $ "OIDC.complete error (" <> show name <> "): " <> show errMsg
  Except.throwError Servant.err403
complete _ name Nothing _ _ _ = do
  liftIO . Diagnose.logMsg $ "OIDC.complete error (" <> show name <> "): no sessId"
  Except.throwError Servant.err403
complete _ name _ _ Nothing _ = do
  liftIO . Diagnose.logMsg $ "OIDC.complete error (" <> show name <> "): no code"
  Except.throwError Servant.err403
complete _ name _ _ _ Nothing = do
  liftIO . Diagnose.logMsg $ "OIDC.complete error (" <> show name <> "): no state"
  Except.throwError Servant.err403
complete env@Env{ jwt } name (Just sessId) Nothing (Just code) (Just state) = do
  pe <- lookupProvider env name
  liftIO $ do
    claims <- codeToClaims pe sessId (Text.encodeUtf8 code) (Text.encodeUtf8 state)
    cookie <- MakeJwt.cookie jwt
      $ Map.singleton (Api.getProviderName name) (Aeson.toJSON claims)
    pure
      $ Servant.addHeader (Text.decodeUtf8 cookie)
      $ Servant.addHeader (Api.Location [URI.relativeReference|/|])
      $ Servant.NoContent
