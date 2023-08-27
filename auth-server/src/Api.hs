module Api where

import Data.Text (Text)

import Servant.API

import qualified Facebook

type SetCookie a = Headers '[Header "Set-Cookie" Text] a

type Facebook =
    ReqBody '[JSON] Facebook.UserToken
    -- https://github.com/haskell-servant/servant/issues/1267
    :> Verb 'POST 204 '[JSON] (SetCookie NoContent)
  :<|> Verb 'DELETE 204 '[JSON] (SetCookie NoContent)

type LoginApi
  = "login" :> "facebook" :> Facebook
