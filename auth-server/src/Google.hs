module Google where
-- https://docs.servant.dev/en/stable/cookbook/open-id-connect/OpenIdConnect.html

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Web.OIDC.Client as OIDC

import qualified Secrets
import qualified Sessions
import qualified Sentry

data ClientSecret = ClientSecret
  { clientId :: BS.ByteString
  , clientSecret :: BS.ByteString
  }

instance Aeson.FromJSON ClientSecret where
  parseJSON = Aeson.withObject "ClientSecret" $ \o -> do
    web <- o .: "web"
    clientId <- Text.encodeUtf8 <$> web .: "client_id"
    clientSecret <- Text.encodeUtf8 <$> web .: "client_secret"
    pure ClientSecret{ clientId, clientSecret }

data Env = Env
  { sessions :: Sessions.Store Sessions.Session
  , httpManager :: HTTP.Manager
  , getProvider :: IO OIDC.Provider
  , secret :: ClientSecret
  , sentry :: Sentry.Service
  }

init :: HTTP.Manager -> IO Env
init httpManager = do
  sessions <- Sessions.newStore
  getProvider <- OIDC.cachedDiscover "https://accounts.google.com" httpManager
  secret <- Secrets.getJson "google_client_secret.json"
  sentry <- Sentry.init
  pure Env
    { sessions
    , httpManager
    , getProvider
    , secret
    , sentry
    }

oidcWithRedirectUri :: Env -> BS.ByteString -> IO OIDC.OIDC
oidcWithRedirectUri Env{ getProvider, secret } redirectUri = do
    OIDC.setCredentials clientId clientSecret redirectUri . OIDC.newOIDC <$> getProvider
  where
    ClientSecret{ clientId, clientSecret } = secret

startUrlForOrigin :: Env -> Text -> IO (Sessions.SessionId, URI.URI)
startUrlForOrigin env@Env{ sessions } origin = do
  sessId <- Sessions.newSessionId
  let
    redirectUri = "https://" <> Text.encodeUtf8 origin <> "/auth/login/google/complete"
    sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
    scopes = [OIDC.openId, OIDC.profile, OIDC.email]
  oidc <- oidcWithRedirectUri env redirectUri
  url <- OIDC.prepareAuthenticationRequestUrl sessionStore oidc scopes []
  pure (sessId, url)

data Claims = Claims
  { email :: Text
  , name :: Text
  , picture :: Text
  , email_verified :: Bool
  -- other keys: at_hash, au, azp, exp, family_name, given_name, iat, iss, nonce, sub
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, Show)

codeToClaims :: Env -> Sessions.SessionId -> BS.ByteString -> BS.ByteString -> IO Claims
codeToClaims env@Env{ sentry, sessions, httpManager } sessId code clientState = do
  Just Sessions.Session{ state = _, nonce = _, redirectUri } <- Sessions.getSession sessId sessions
  let
    sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
  oidc <- oidcWithRedirectUri env redirectUri
  OIDC.Tokens { idToken = OIDC.IdTokenClaims { otherClaims } }
    <- Sentry.reportException sentry
    $ OIDC.getValidTokens sessionStore oidc httpManager clientState code
  pure otherClaims
