module Google where
-- https://docs.servant.dev/en/stable/cookbook/open-id-connect/OpenIdConnect.html

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI as URI
import qualified Web.OIDC.Client as OIDC

import qualified Diagnose
import qualified Secrets
import qualified Sessions

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
  { sessions :: Sessions.Store
  , httpManager :: HTTP.Manager
  , provider :: OIDC.Provider
  , secret :: ClientSecret
  }

getSecret :: IO ClientSecret
getSecret = do
  secretsDir <- Secrets.getDir
  Right secret
    <- Aeson.eitherDecode <$> BSL.readFile (secretsDir <> "/google_client_secret.json")
  pure secret

init :: IO Env
init = do
  httpManager <- HTTP.newManager HTTPS.tlsManagerSettings
  provider <- OIDC.discover "https://accounts.google.com" httpManager
  sessions <- Sessions.newStore
  secret <- getSecret
  pure Env
    { sessions
    , httpManager
    , provider
    , secret
    }

oidcWithRedirectUri :: Env -> BS.ByteString -> OIDC.OIDC
oidcWithRedirectUri Env{ provider, secret } redirectUri =
    OIDC.setCredentials clientId clientSecret redirectUri (OIDC.newOIDC provider)
  where
    ClientSecret{ clientId, clientSecret } = secret

startUrlForOrigin :: Env -> Text -> IO (Sessions.SessionId, URI.URI)
startUrlForOrigin env@Env{ sessions } origin = do
  sessId <- Sessions.newSessionId
  let
    redirectUri = "https://" <> Text.encodeUtf8 origin <> "/auth/login/google/complete"
    oidc = oidcWithRedirectUri env redirectUri
    sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
    scopes = [OIDC.openId, OIDC.profile, OIDC.email]
  url <- OIDC.prepareAuthenticationRequestUrl sessionStore oidc scopes []
  pure (sessId, url)

data Claims = Claims
  { email :: Text
  , name :: Text
  , picture :: Text
  , email_verified :: Bool
  -- other keys: at_hash, au, azp, exp, family_name, given_name, iat, iss, nonce, sub
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, Show)

codeToClaims :: Env -> Sessions.SessionId -> BS.ByteString -> IO Claims
codeToClaims env@Env{ sessions, httpManager } sessId code = do
  Just Sessions.Session{ state, nonce = _, redirectUri } <- Sessions.getSession sessId sessions
  let
    oidc = oidcWithRedirectUri env redirectUri
    sessionStore = Sessions.oidcSessionStore sessions sessId redirectUri
  OIDC.Tokens { idToken = OIDC.IdTokenClaims { otherClaims } }
    <- Diagnose.annotateException "codeToClaims/getValidTokens" $ OIDC.getValidTokens sessionStore oidc httpManager state code
  pure otherClaims
