{-# LANGUAGE QuasiQuotes #-}
module AuthServer (app, main) where

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (Proxy))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.IO as IO

import Servant ((:<|>) ((:<|>)))
import qualified Servant
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI.Static as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors

import qualified Api
import qualified Bluesky
import qualified Diagnose
import qualified Facebook
import qualified Friendica
import qualified Google
import qualified MakeJwt
import qualified Sessions

data Env = Env
  { bluesky :: Bluesky.Env
  , friendica :: Friendica.Env
  , google :: Google.Env
  , jwt :: MakeJwt.Env
  }

doInit :: IO Env
doInit = do
  sessions <- Sessions.newStore
  httpManager <- HTTP.newManager HTTPS.tlsManagerSettings
  jwt <- MakeJwt.init
  bluesky <- Bluesky.init httpManager
  friendica <- Friendica.init sessions httpManager jwt
  google <- Google.init sessions httpManager
  pure Env{ bluesky, friendica, google, jwt }

facebookLogin :: MakeJwt.Env -> Facebook.UserToken -> Servant.Handler (Api.SetCookie Servant.NoContent)
facebookLogin jwtEnv userToken = do
  cookie <- liftIO $ do
    fbUserId <- Facebook.getUserId userToken
    MakeJwt.cookie jwtEnv (Map.singleton "facebookUserId" (Aeson.toJSON fbUserId))
  pure $ Servant.addHeader (Text.decodeUtf8 cookie) Servant.NoContent

logout :: Servant.Handler (Api.SetCookie Servant.NoContent)
logout =
  pure $ Servant.addHeader "jwt=; Path=/; Max-Age=0" Servant.NoContent

googleStart :: Google.Env -> Maybe Text -> Servant.Handler Api.CookieRedirect
googleStart _ Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
googleStart env (Just host) = do
  (sessId, url) <- liftIO $ Google.startUrlForOrigin env host
  pure
    $ Servant.addHeader (Sessions.sessionIdCookie sessId)
    $ Servant.addHeader (Api.Location url)
    $ Servant.NoContent

googleComplete :: Env -> Maybe Sessions.SessionId -> Maybe Text -> Maybe Text -> Maybe Text -> Servant.Handler Api.CookieRedirect
googleComplete _ _ (Just errMsg) _ _ = do
  liftIO . Diagnose.logMsg $ "googleComplete error: " <> show errMsg
  Except.throwError Servant.err403
googleComplete _ Nothing _ _ _ = do
  liftIO . Diagnose.logMsg $ "googleComplete error: no sessId"
  Except.throwError Servant.err403
googleComplete _ _ _ Nothing _ = do
  liftIO . Diagnose.logMsg $ "googleComplete error: no code"
  Except.throwError Servant.err403
googleComplete _ _ _ _ Nothing = do
  liftIO . Diagnose.logMsg $ "googleComplete error: no state"
  Except.throwError Servant.err403
googleComplete Env{ google, jwt } (Just sessId) Nothing (Just code) (Just state) = do
  liftIO $ do
    claims <- Google.codeToClaims google sessId (Text.encodeUtf8 code) (Text.encodeUtf8 state)
    cookie <- MakeJwt.cookie jwt (Map.singleton "google" (Aeson.toJSON claims))
    pure
      $ Servant.addHeader (Text.decodeUtf8 cookie)
      $ Servant.addHeader (Api.Location [URI.relativeReference|/|])
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
server env@Env{ bluesky, friendica, google, jwt } =
  loginServer
  :<|> facebookDecodeSignedReq
  :<|> Bluesky.serveClientMetadata
  where
    loginServer =
      logout
      :<|> facebookLogin jwt
      :<|> (googleStart google :<|> googleComplete env)
      :<|> (Friendica.start friendica :<|> Friendica.complete friendica)
      :<|> Bluesky.start bluesky

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
