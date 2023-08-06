module AuthServer (app, main) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))

import qualified Servant
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors

import qualified Api
import qualified Facebook
import qualified MakeJwt

facebook :: Facebook.UserToken -> Servant.Handler (Api.SetCookie ())
facebook userToken = do
  jwt <- liftIO $ MakeJwt.makeJwt =<< Facebook.getUserId userToken
  pure $ Servant.addHeader ("jwt=" <> jwt <> "; Path=/; HttpOnly; Max-Age=86400") ()

loginServer :: Servant.Server Api.LoginApi
loginServer = facebook

app :: Wai.Application
app = Servant.serve (Proxy @Api.LoginApi) loginServer

main :: IO ()
main = Warp.run 5001 (Cors.simpleCors app)
