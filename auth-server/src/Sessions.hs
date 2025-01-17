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
randomString = replicateM 40 randomChar
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

sessionIdCookie :: BS.ByteString -> SessionId -> Text
sessionIdCookie path (SessionId sessId) =
  Text.decodeUtf8 $ Cookie.renderSetCookieBS Cookie.defaultSetCookie
    { Cookie.setCookieName = sessionIdKey
    , Cookie.setCookieValue = Text.encodeUtf8 sessId
    , Cookie.setCookiePath = Just path
    , Cookie.setCookieMaxAge = Just 3600
    , Cookie.setCookieHttpOnly = True
    , Cookie.setCookieSecure = True
    }

type Store s = IORef.IORef (Map.Map SessionId s)

newStore :: IO (Store s)
newStore = IORef.newIORef Map.empty

getSession :: SessionId -> Store s -> IO (Maybe s)
getSession sessId store = Map.lookup sessId <$> IORef.readIORef store

setSession :: SessionId -> s -> Store s -> IO ()
setSession sessId session store =
  IORef.atomicModifyIORef store $ \sessMap -> (Map.insert sessId session sessMap, ())

deleteSession :: SessionId -> Store s -> IO ()
deleteSession sessId store =
  IORef.atomicModifyIORef store $ \sessMap -> (Map.delete sessId sessMap, ())

data Session = Session
  { state :: BS.ByteString
  , nonce :: BS.ByteString
  , redirectUri :: BS.ByteString
  }

oidcSessionStore :: Store Session -> SessionId -> BS.ByteString -> OIDC.SessionStore IO
oidcSessionStore store sessId redirectUri =
  let
    sessionStoreSave state nonce =
      setSession sessId Session{ state, nonce, redirectUri } store
    sessionStoreGet clientState = do
      found <- getSession sessId store
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
    sessionStoreDelete _ = deleteSession sessId store
  in
  OIDC.SessionStore
    { sessionStoreGenerate = BSC.pack <$> randomString
    , sessionStoreSave
    , sessionStoreGet
    , sessionStoreDelete
    }
