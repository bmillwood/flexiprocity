module OIDC where
-- https://docs.servant.dev/en/stable/cookbook/open-id-connect/OpenIdConnect.html

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Web.OIDC.Client as OC

import qualified Api
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

newtype Env = Env (Map Api.ProviderName ProviderEnv)

basePath :: BS.ByteString
basePath = "/auth/login/oidc"

redirectUriRelative :: Api.ProviderName -> BS.ByteString
redirectUriRelative name =
  basePath <> "/complete/" <> Text.encodeUtf8 (Api.getProviderName name)

init :: HTTP.Manager -> Sentry.Service -> IO Env
init httpManager sentry = do
  providers <- Secrets.getJson "oidc_providers.json"
  Env <$> Map.traverseWithKey (initProvider httpManager sentry) providers

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

lookup :: Env -> Api.ProviderName -> Maybe ProviderEnv
lookup (Env providers) name = Map.lookup name providers

oidcWithRedirectUri :: ProviderEnv -> BS.ByteString -> IO OC.OIDC
oidcWithRedirectUri ProviderEnv{ getProvider, provider = Provider{ clientId, clientSecret } } redirectUri =
    OC.setCredentials (Text.encodeUtf8 clientId) (Text.encodeUtf8 clientSecret) redirectUri
      . OC.newOIDC <$> getProvider

startUrlForOrigin :: ProviderEnv -> Text -> IO (Sessions.SessionId, URI.URI)
startUrlForOrigin env@ProviderEnv{ providerName, sessions } origin = do
  sessId <- Sessions.newSessionId
  let
    redirectUri =
      "https://" <> Text.encodeUtf8 origin <> redirectUriRelative providerName
    sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
    scopes = [OC.openId, OC.profile, OC.email]
  oidc <- oidcWithRedirectUri env redirectUri
  url <- OC.prepareAuthenticationRequestUrl sessionStore oidc scopes []
  pure (sessId, url)

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
