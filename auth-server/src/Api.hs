module Api where

import Data.Text (Text)

import Servant.API

import qualified Facebook

type SetCookie a = Headers '[Header "Set-Cookie" Text] a

type LoginApi
  = "login"
    :> "facebook"
    :> ReqBody '[JSON] Facebook.UserToken
    :> Post '[JSON] (SetCookie ())
