module AuthServer (app, main) where

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (Proxy))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

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

jwtCookie :: Map Text Aeson.Value -> IO Text
jwtCookie claimsMap = cookie <$> MakeJwt.makeJwt claimsMap
  where
    cookie jwt =
      Text.decodeUtf8 $ Cookie.renderSetCookieBS Cookie.defaultSetCookie
        { Cookie.setCookieName = "jwt"
        , Cookie.setCookieValue = Text.encodeUtf8 jwt
        , Cookie.setCookiePath = Just "/"
        , Cookie.setCookieMaxAge = Just 86400
        , Cookie.setCookieHttpOnly = True
        , Cookie.setCookieSecure = True
        , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
        }

facebookLogin :: Facebook.UserToken -> Servant.Handler (Api.SetCookie Servant.NoContent)
facebookLogin userToken = do
  fbUserId <- liftIO $ Facebook.getUserId userToken
  cookie <- liftIO $ jwtCookie (Map.singleton "facebookUserId" (Aeson.toJSON fbUserId))
  pure $ Servant.addHeader cookie Servant.NoContent

logout :: Servant.Handler (Api.SetCookie Servant.NoContent)
logout =
  pure $ Servant.addHeader "jwt=; Path=/; Max-Age=0" Servant.NoContent

googleStart :: Google.Env -> Servant.Handler Api.CookieRedirect
googleStart env = do
  (sessId, url) <- liftIO $ Google.startUrl env
  pure
    $ Servant.addHeader (Google.sessionIdCookie sessId)
    $ Servant.addHeader url
    $ Servant.NoContent

googleComplete :: Google.Env -> Maybe Google.SessionId -> Maybe Text -> Maybe Text -> Servant.Handler Api.CookieRedirect
googleComplete _ _ (Just errMsg) _ = do
  liftIO . putStrLn $ "googleComplete error: " <> show errMsg
  Except.throwError Servant.err403
googleComplete _ Nothing _ _ = do
  liftIO . putStrLn $ "googleComplete error: no sessId"
  Except.throwError Servant.err403
googleComplete _ _ _ Nothing = do
  liftIO . putStrLn $ "googleComplete error: no code"
  Except.throwError Servant.err403
googleComplete env (Just sessId) Nothing (Just code) = do
  email <- liftIO $ Google.codeToEmail env sessId (Text.encodeUtf8 code)
  cookie <- liftIO $ jwtCookie (Map.singleton "googleEmail" (Aeson.toJSON email))
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

server :: Google.Env -> Servant.Server Api.Api
server google = loginServer :<|> facebookDecodeSignedReq
  where
    loginServer =
      logout
      :<|> facebookLogin
      :<|> (googleStart google :<|> googleComplete google)

app :: Google.Env -> Wai.Application
app google = Servant.serve (Proxy @Api.Api) (server google)

main :: IO ()
main = do
  google <- Google.init
  Warp.run 5001 (Cors.simpleCors (app google))
