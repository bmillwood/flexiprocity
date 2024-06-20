{-# LANGUAGE DisambiguateRecordFields #-}
module Google where
-- https://docs.servant.dev/en/stable/cookbook/open-id-connect/OpenIdConnect.html

import Control.Monad
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Servant.API as Servant
import qualified System.Environment as Env
import qualified System.Random.Stateful as Random
import qualified Web.Cookie as Cookie
import qualified Web.OIDC.Client as OIDC

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

newtype SessionId = SessionId Text deriving (Eq, Ord)

sessionIdKey :: BS.ByteString
sessionIdKey = "sessionId"

instance Servant.FromHttpApiData SessionId where
  parseUrlPiece piece = Left ("parseUrlPiece " <> Text.pack (show piece))
  parseHeader header =
    case lookup sessionIdKey (Cookie.parseCookies header) of
      Nothing -> Left "no session"
      Just c -> Right (SessionId (Text.decodeUtf8 c))
  parseQueryParam qp = Left ("parseQueryParam " <> Text.pack (show qp))

data Session = Session
  { state :: BS.ByteString
  , nonce :: BS.ByteString
  }

data Env = Env
  { oidc :: OIDC.OIDC
  , sessions :: IORef.IORef (Map.Map SessionId Session)
  , httpManager :: HTTP.Manager
  , provider :: OIDC.Provider
  , redirectUri :: BS.ByteString
  , secret :: ClientSecret
  }

init :: IO Env
init = do
  origin <- BSC.pack <$> Env.getEnv "HOST_ORIGIN"
  httpManager <- HTTP.newManager HTTPS.tlsManagerSettings
  provider <- OIDC.discover "https://accounts.google.com" httpManager
  sessions <- IORef.newIORef Map.empty
  Right secret@ClientSecret{ clientId, clientSecret }
    <- Aeson.eitherDecode <$> BSL.readFile "../secrets/google_client_secret.json"
  let
    redirectUri = "https://" <> origin <> "/auth/login/google/complete"
    oidc = OIDC.setCredentials clientId clientSecret redirectUri (OIDC.newOIDC provider)
  pure Env
    { oidc
    , sessions
    , httpManager
    , provider
    , redirectUri
    , secret
    }

randomString :: IO String
randomString = replicateM 80 randomChar
  where
    randomChars = BSC.pack $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    randomChar :: IO Char
    randomChar = BSC.index randomChars <$> Random.uniformRM (0, BS.length randomChars - 1) Random.globalStdGen

getSessionStore :: IORef.IORef (Map.Map SessionId Session) -> SessionId -> OIDC.SessionStore IO
getSessionStore ref sessId =
  let
    sessionStoreSave state nonce = do
      IORef.atomicModifyIORef ref $ \sessMap -> (Map.insert sessId Session{ state, nonce } sessMap, ())
    sessionStoreGet clientState = do
      found <- Map.lookup sessId <$> IORef.readIORef ref
      pure $ found >>= \Session{ state, nonce } ->
        if state == clientState
          then Just nonce
          else Nothing
    -- AIUI this is only done when authentication succeeds. If people partially
    -- complete authentication and then abandon it, the session will be in the
    -- map forever. I expect this to be rare enough I can just ignore it for
    -- now, but we'll eventually need to address it somehow. (Perhaps by using
    -- some "real" session storage mechanism, rather than "thing I hacked
    -- together quickly to get this working".)
    sessionStoreDelete = IORef.atomicModifyIORef ref $ \sessMap -> (Map.delete sessId sessMap, ())
  in
  OIDC.SessionStore
    { sessionStoreGenerate = BSC.pack <$> randomString
    , sessionStoreSave
    , sessionStoreGet
    , sessionStoreDelete
    }

startUrl :: Env -> IO (SessionId, Text)
startUrl Env{ oidc, sessions } = do
  sessId <- SessionId . Text.pack <$> randomString
  url <- Text.pack . show <$> OIDC.prepareAuthenticationRequestUrl (getSessionStore sessions sessId) oidc [OIDC.openId, OIDC.email] []
  pure (sessId, url)

sessionIdCookie :: SessionId -> Text
sessionIdCookie (SessionId sessId) =
  Text.decodeUtf8 $ Cookie.renderSetCookieBS Cookie.defaultSetCookie
    { Cookie.setCookieName = sessionIdKey
    , Cookie.setCookieValue = Text.encodeUtf8 sessId
    , Cookie.setCookiePath = Just "/"
    , Cookie.setCookieMaxAge = Just 300
    , Cookie.setCookieHttpOnly = True
    , Cookie.setCookieSecure = True
    }

data Claims = Claims
  { email :: Text
  } deriving (Generic, Aeson.FromJSON)

codeToEmail :: Env -> SessionId -> BS.ByteString -> IO Text
codeToEmail Env{ oidc, sessions, httpManager } sessId code = do
  Just Session{ state, nonce = _ } <- Map.lookup sessId <$> IORef.readIORef sessions
  OIDC.Tokens { idToken = OIDC.IdTokenClaims { otherClaims = Claims { email } } }
    <- OIDC.getValidTokens (getSessionStore sessions sessId) oidc httpManager state code
  pure email
