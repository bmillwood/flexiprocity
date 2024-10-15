module Sessions where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified System.Random.Stateful as Random
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified Servant.API as Servant
import qualified Web.Cookie as Cookie
import qualified Web.OIDC.Client as OIDC

newtype SessionId = SessionId Text deriving (Eq, Ord)

sessionIdKey :: BS.ByteString
sessionIdKey = "sessionId"

randomString :: IO String
randomString = replicateM 80 randomChar
  where
    randomChars = BSC.pack $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    randomChar :: IO Char
    randomChar = BSC.index randomChars <$> Random.uniformRM (0, BS.length randomChars - 1) Random.globalStdGen

newSessionId :: IO SessionId
newSessionId = SessionId . Text.pack <$> randomString

instance Servant.FromHttpApiData SessionId where
  parseUrlPiece piece = Left ("parseUrlPiece " <> Text.pack (show piece))
  parseHeader header =
    case lookup sessionIdKey (Cookie.parseCookies header) of
      Nothing -> Left "no session"
      Just c -> Right (SessionId (Text.decodeUtf8 c))
  parseQueryParam qp = Left ("parseQueryParam " <> Text.pack (show qp))

sessionIdCookie :: SessionId -> Text
sessionIdCookie (SessionId sessId) =
  Text.decodeUtf8 $ Cookie.renderSetCookieBS Cookie.defaultSetCookie
    { Cookie.setCookieName = sessionIdKey
    , Cookie.setCookieValue = Text.encodeUtf8 sessId
    , Cookie.setCookiePath = Just "/"
    , Cookie.setCookieMaxAge = Just 3600
    , Cookie.setCookieHttpOnly = True
    , Cookie.setCookieSecure = True
    }

data Session = Session
  { state :: BS.ByteString
  , nonce :: BS.ByteString
  , redirectUri :: BS.ByteString
  }

type Store = IORef.IORef (Map.Map SessionId Session)

newStore :: IO Store
newStore = IORef.newIORef Map.empty

getSession :: SessionId -> Store -> IO (Maybe Session)
getSession sid store = Map.lookup sid <$> IORef.readIORef store

oidcSessionStore :: Store -> SessionId -> BS.ByteString -> OIDC.SessionStore IO
oidcSessionStore ref sessId redirectUri =
  let
    sessionStoreSave state nonce = do
      IORef.atomicModifyIORef ref $ \sessMap -> (Map.insert sessId Session{ state, nonce, redirectUri } sessMap, ())
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
