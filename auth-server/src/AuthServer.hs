module AuthServer (app, main) where

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Servant ((:<|>) ((:<|>)))
import qualified Servant
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors

import qualified Api
import qualified Facebook
import qualified MakeJwt

facebookLogin :: Facebook.UserToken -> Servant.Handler (Api.SetCookie Servant.NoContent)
facebookLogin userToken = do
  jwt <- liftIO $ MakeJwt.makeJwt =<< Facebook.getUserId userToken
  pure $ Servant.addHeader (cookie jwt) Servant.NoContent
  where
    cookie jwt =
      Text.intercalate "; "
      $ [ "jwt=" <> jwt
        , "Path=/"
        , "HttpOnly"
        , "SameSite=Lax"
        , "Secure"
        , "Max-Age=86400"
        ]

facebookLogout :: Servant.Handler (Api.SetCookie Servant.NoContent)
facebookLogout =
  pure $ Servant.addHeader "jwt=; Path=/; Max-Age=0" Servant.NoContent

facebookDecodeSignedReq :: Facebook.SignedRequest -> Servant.Handler Aeson.Value
facebookDecodeSignedReq signedReq = do
  result <- liftIO $ Facebook.decodeSignedRequest signedReq
  case result of
    Left err ->
      Except.throwError Servant.err400{ Servant.errBody = bsFromString err }
    Right value -> pure value
  where
    bsFromString = BSL.fromStrict . Text.encodeUtf8 . Text.pack

loginServer :: Servant.Server Api.Api
loginServer =
  (facebookLogin :<|> facebookLogout)
  :<|> facebookDecodeSignedReq

app :: Wai.Application
app = Servant.serve (Proxy @Api.Api) loginServer

main :: IO ()
main = Warp.run 5001 (Cors.simpleCors app)
