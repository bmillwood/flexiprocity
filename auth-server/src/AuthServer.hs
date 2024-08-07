module AuthServer (app, main) where

import Control.Exception
import qualified Control.Monad.Except as Except
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (Proxy))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time

import Servant ((:<|>) ((:<|>)))
import qualified Servant
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Web.Cookie as Cookie

import qualified Api
import qualified Facebook
import qualified Google
import qualified MakeJwt

data Env = Env
  { google :: Google.Env
  , jwt :: MakeJwt.Env
  }

doInit :: IO Env
doInit = Env <$> Google.init <*> MakeJwt.init

addTimestamp :: String -> IO String
addTimestamp s = do
  now <- Time.getCurrentTime
  pure $ "[" <> show now <> "] " <> s

logMsg :: String -> IO ()
logMsg = putStrLn <=< addTimestamp

jwtCookie :: MakeJwt.Env -> Map Text Aeson.Value -> IO Text
jwtCookie jwtEnv claimsMap = cookie <$> MakeJwt.makeJwt jwtEnv claimsMap
  where
    cookie encodedJwt =
      Text.decodeUtf8 $ Cookie.renderSetCookieBS Cookie.defaultSetCookie
        { Cookie.setCookieName = "jwt"
        , Cookie.setCookieValue = encodedJwt
        , Cookie.setCookiePath = Just "/"
        , Cookie.setCookieMaxAge = Just 86400
        , Cookie.setCookieHttpOnly = True
        , Cookie.setCookieSecure = True
        , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
        }

facebookLogin :: MakeJwt.Env -> Facebook.UserToken -> Servant.Handler (Api.SetCookie Servant.NoContent)
facebookLogin jwtEnv userToken = do
  cookie <- liftIO $ do
    fbUserId <- Facebook.getUserId userToken
    jwtCookie jwtEnv (Map.singleton "facebookUserId" (Aeson.toJSON fbUserId))
  pure $ Servant.addHeader cookie Servant.NoContent

logout :: Servant.Handler (Api.SetCookie Servant.NoContent)
logout =
  pure $ Servant.addHeader "jwt=; Path=/; Max-Age=0" Servant.NoContent

googleStart :: Google.Env -> Maybe Text -> Servant.Handler Api.CookieRedirect
googleStart _ Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
googleStart env (Just host) = do
  (sessId, url) <- liftIO $ Google.startUrlForOrigin env host
  pure
    $ Servant.addHeader (Google.sessionIdCookie sessId)
    $ Servant.addHeader url
    $ Servant.NoContent

googleComplete :: Env -> Maybe Google.SessionId -> Maybe Text -> Maybe Text -> Servant.Handler Api.CookieRedirect
googleComplete _ _ (Just errMsg) _ = do
  liftIO . logMsg $ "googleComplete error: " <> show errMsg
  Except.throwError Servant.err403
googleComplete _ Nothing _ _ = do
  liftIO . logMsg $ "googleComplete error: no sessId"
  Except.throwError Servant.err403
googleComplete _ _ _ Nothing = do
  liftIO . logMsg $ "googleComplete error: no code"
  Except.throwError Servant.err403
googleComplete Env{ google, jwt } (Just sessId) Nothing (Just code) = do
  liftIO $ do
    claims <- Google.codeToClaims google sessId (Text.encodeUtf8 code)
      `catch` \e@SomeException{} -> logMsg ("codeToClaims: " <> show e) >> throwIO e
    cookie <- jwtCookie jwt (Map.singleton "google" (Aeson.toJSON claims))
      `catch` \e@SomeException{} -> logMsg ("cookie: " <> show e) >> throwIO e
    pure
      $ Servant.addHeader cookie
      $ Servant.addHeader "/"
      $ Servant.NoContent

facebookDecodeSignedReq :: Facebook.SignedRequest -> Servant.Handler Aeson.Value
facebookDecodeSignedReq signedReq = do
  result <- liftIO $ Facebook.decodeSignedRequest signedReq
  case result of
    Left err ->
      Except.throwError Servant.err400{ Servant.errBody = bsFromString err }
    Right value -> pure value
  where
    bsFromString = BSL.fromStrict . Text.encodeUtf8 . Text.pack

server :: Env -> Servant.Server Api.Api
server env@Env{ google, jwt } = loginServer :<|> facebookDecodeSignedReq
  where
    loginServer =
      logout
      :<|> facebookLogin jwt
      :<|> (googleStart google :<|> googleComplete env)

app :: Env -> Wai.Application
app env = Servant.serve (Proxy @Api.Api) (server env)

main :: IO ()
main = do
  env <- doInit
  logMsg $ "Initialization finished"
  Warp.run 5001 (Cors.simpleCors (app env))
