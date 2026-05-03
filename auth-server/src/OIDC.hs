module OIDC where
-- https://docs.servant.dev/en/stable/cookbook/open-id-connect/OpenIdConnect.html

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Web.OIDC.Client as OC

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
  , getProvider :: IO OC.Provider
  , secret :: ClientSecret
  , sentry :: Sentry.Service
  }

init :: HTTP.Manager -> IO Env
init httpManager = do
  sessions <- Sessions.newStore
  getProvider <- OC.cachedDiscover "https://accounts.google.com" httpManager
  secret <- Secrets.getJson "google_client_secret.json"
  sentry <- Sentry.init
  pure Env
    { sessions
    , httpManager
    , getProvider
    , secret
    , sentry
    }

oidcWithRedirectUri :: Env -> BS.ByteString -> IO OC.OIDC
oidcWithRedirectUri Env{ getProvider, secret } redirectUri = do
    OC.setCredentials clientId clientSecret redirectUri . OC.newOIDC <$> getProvider
  where
    ClientSecret{ clientId, clientSecret } = secret

startUrlForOrigin :: Env -> Text -> IO (Sessions.SessionId, URI.URI)
startUrlForOrigin env@Env{ sessions } origin = do
  sessId <- Sessions.newSessionId
  let
    redirectUri = "https://" <> Text.encodeUtf8 origin <> "/auth/login/google/complete"
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

codeToClaims :: Env -> Sessions.SessionId -> BS.ByteString -> BS.ByteString -> IO Claims
codeToClaims env@Env{ sentry, sessions, httpManager } sessId code clientState = do
  Just Sessions.Session{ state = _, nonce = _, redirectUri } <- Sessions.getSession sessId sessions
  let
    sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
  oidc <- oidcWithRedirectUri env redirectUri
  OC.Tokens { idToken = OC.IdTokenClaims { otherClaims } }
    <- Sentry.reportException sentry
    $ OC.getValidTokens sessionStore oidc httpManager clientState code
  pure otherClaims
