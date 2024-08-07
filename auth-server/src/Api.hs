module Api where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import Servant.API

import qualified Facebook
import qualified Google

type SetCookie a = Headers '[Header "Set-Cookie" Text] a

type LoginFacebook =
    ReqBody '[JSON] Facebook.UserToken
    -- https://github.com/haskell-servant/servant/issues/1267
    :> Verb 'POST 204 '[JSON] (SetCookie NoContent)

type CookieRedirect = Headers '[Header "Set-Cookie" Text, Header "Location" Text] NoContent

type LoginGoogle =
  "start"
    :> Header "X-Forwarded-Host" Text
    :> Verb 'GET 303 '[PlainText] CookieRedirect
  :<|> "complete"
    :> Header "Cookie" Google.SessionId
    :> QueryParam "error" Text
    :> QueryParam "code" Text
    :> Verb 'GET 303 '[JSON] CookieRedirect

type FacebookDecodeSignedRequest =
  ReqBody '[JSON] Facebook.SignedRequest
  :> Get '[JSON] Aeson.Value

type Api =
  "login" :> (
    Verb 'DELETE 204 '[JSON] (SetCookie NoContent)
    :<|> "facebook" :> LoginFacebook
    :<|> "google" :> LoginGoogle
  )
  :<|> "facebook" :> "decode-signed-request" :> FacebookDecodeSignedRequest
