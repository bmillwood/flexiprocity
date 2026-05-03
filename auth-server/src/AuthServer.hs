module AuthServer (app, main) where

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (Proxy))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.IO as IO

import Servant ((:<|>) ((:<|>)))
import qualified Servant
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors

import qualified Api
import qualified Bluesky
import qualified Diagnose
import qualified Facebook
import qualified Friendica
import qualified MakeJwt
import qualified OIDC
import qualified Sentry

data Env = Env
  { bluesky :: Bluesky.Env
  , friendica :: Friendica.Env
  , oidc :: OIDC.Env
  , jwt :: MakeJwt.Env
  }

doInit :: IO Env
doInit = do
  httpManager <- HTTP.newManager HTTPS.tlsManagerSettings
  sentry <- Sentry.init
  jwt <- MakeJwt.init
  bluesky <- Bluesky.init httpManager jwt
  friendica <- Friendica.init httpManager jwt
  oidc <- OIDC.init httpManager sentry jwt
  pure Env{ bluesky, friendica, oidc, jwt }

facebookLogin :: MakeJwt.Env -> Facebook.UserToken -> Servant.Handler (Api.SetCookie Servant.NoContent)
facebookLogin jwtEnv userToken = do
  cookie <- liftIO $ do
    fbUserId <- Facebook.getUserId userToken
    MakeJwt.cookie jwtEnv (Map.singleton "facebookUserId" (Aeson.toJSON fbUserId))
  pure $ Servant.addHeader (Text.decodeUtf8 cookie) Servant.NoContent

logout :: Servant.Handler (Api.SetCookie Servant.NoContent)
logout =
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

server :: Env -> Servant.Server Api.Api
server Env{ bluesky, friendica, oidc, jwt } =
  loginServer
  :<|> facebookDecodeSignedReq
  :<|> Bluesky.serveClientMetadata bluesky
  where
    loginServer =
      logout
      :<|> facebookLogin jwt
      :<|> (OIDC.start oidc :<|> OIDC.complete oidc)
      :<|> (Friendica.start friendica :<|> Friendica.complete friendica)
      :<|> (Bluesky.start bluesky :<|> Bluesky.complete bluesky)

app :: Env -> Wai.Application
app env = Servant.serve (Proxy @Api.Api) (server env)

main :: IO ()
main = do
  IO.hGetBuffering IO.stdout
    >>= Diagnose.logMsg . ("stdout buffering was: " <>) . show
  IO.hSetBuffering IO.stdout IO.LineBuffering
  env <- doInit
  Diagnose.logMsg $ "Initialization finished"
  Warp.runEnv 5001 (Cors.simpleCors (app env))
