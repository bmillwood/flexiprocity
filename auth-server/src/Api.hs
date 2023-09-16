module Api where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import Servant.API

import qualified Facebook

type SetCookie a = Headers '[Header "Set-Cookie" Text] a

type LoginFacebook =
    ReqBody '[JSON] Facebook.UserToken
    -- https://github.com/haskell-servant/servant/issues/1267
    :> Verb 'POST 204 '[JSON] (SetCookie NoContent)
  :<|> Verb 'DELETE 204 '[JSON] (SetCookie NoContent)

type FacebookDecodeSignedRequest =
  ReqBody '[JSON] Facebook.SignedRequest
  :> Get '[JSON] Aeson.Value

type Api
  = ("login" :> "facebook" :> LoginFacebook)
  :<|> ("facebook" :> "decode-signed-request" :> FacebookDecodeSignedRequest)
