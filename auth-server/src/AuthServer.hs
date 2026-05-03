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
import qualified MakeJwt
import qualified OIDC
import qualified Sentry
import qualified Sessions

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
  oidc <- OIDC.init httpManager sentry
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

lookupOidc :: OIDC.Env -> Api.ProviderName -> Servant.Handler OIDC.ProviderEnv
lookupOidc oidcEnv name =
  maybe (Except.throwError Servant.err404) pure $ OIDC.lookup oidcEnv name

oidcStart :: OIDC.Env -> Api.ProviderName -> Maybe Text -> Servant.Handler Api.CookieRedirect
oidcStart _ _ Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
oidcStart oidcEnv name (Just host) = do
  pe <- lookupOidc oidcEnv name
  (sessId, url) <- liftIO $ OIDC.startUrlForOrigin pe host
  pure
    $ Servant.addHeader (Sessions.sessionIdCookie (OIDC.redirectUriRelative name) sessId)
    $ Servant.addHeader (Api.Location url)
    $ Servant.NoContent

oidcComplete
  :: Env
  -> Api.ProviderName
  -> Maybe Sessions.SessionId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Servant.Handler Api.CookieRedirect
oidcComplete _ name _ (Just errMsg) _ _ = do
  liftIO . Diagnose.logMsg $ "oidcComplete error (" <> show name <> "): " <> show errMsg
  Except.throwError Servant.err403
oidcComplete _ name Nothing _ _ _ = do
  liftIO . Diagnose.logMsg $ "oidcComplete error (" <> show name <> "): no sessId"
  Except.throwError Servant.err403
oidcComplete _ name _ _ Nothing _ = do
  liftIO . Diagnose.logMsg $ "oidcComplete error (" <> show name <> "): no code"
  Except.throwError Servant.err403
oidcComplete _ name _ _ _ Nothing = do
  liftIO . Diagnose.logMsg $ "oidcComplete error (" <> show name <> "): no state"
  Except.throwError Servant.err403
oidcComplete Env{ oidc, jwt } name (Just sessId) Nothing (Just code) (Just state) = do
  pe <- lookupOidc oidc name
  liftIO $ do
    claims <- OIDC.codeToClaims pe sessId (Text.encodeUtf8 code) (Text.encodeUtf8 state)
    cookie <- MakeJwt.cookie jwt
      $ Map.singleton (Api.getProviderName name) (Aeson.toJSON claims)
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
server env@Env{ bluesky, friendica, oidc, jwt } =
  loginServer
  :<|> facebookDecodeSignedReq
  :<|> Bluesky.serveClientMetadata bluesky
  where
    loginServer =
      logout
      :<|> facebookLogin jwt
      :<|> (oidcStart oidc :<|> oidcComplete env)
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
