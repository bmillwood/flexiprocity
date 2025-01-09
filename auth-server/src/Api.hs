module Api where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Bluesky.Handle as Bluesky
import Servant.API
import qualified Network.URI as URI

import qualified Facebook
import qualified Sessions

type SetCookie a = Headers '[Header "Set-Cookie" Text] a

type LoginFacebook =
    ReqBody '[JSON] Facebook.UserToken
    -- https://github.com/haskell-servant/servant/issues/1267
    :> Verb 'POST 204 '[JSON] (SetCookie NoContent)

newtype Location = Location URI.URI

instance ToHttpApiData Location where
  toUrlPiece (Location uri) = Text.pack (URI.uriToString id uri "")

type CookieRedirect = Headers '[Header "Set-Cookie" Text, Header "Location" Location] NoContent

type LoginGoogle =
  "start"
    :> Header "X-Forwarded-Host" Text
    :> Verb 'GET 303 '[PlainText] CookieRedirect
  :<|> "complete"
    :> Header "Cookie" Sessions.SessionId
    :> QueryParam "error" Text
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Verb 'GET 303 '[JSON] CookieRedirect

newtype InstanceName = InstanceName { getInstanceName :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey, Aeson.ToJSONKey, FromHttpApiData)

type LoginFriendica =
  "start"
    :> Capture "instance" InstanceName
    :> Header "X-Forwarded-Host" Text
    :> Verb 'GET 303 '[PlainText] CookieRedirect
  :<|> "complete"
    :> Capture "instance" InstanceName
    :> Header "Cookie" Sessions.SessionId
    :> QueryParam "error" Text
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Verb 'GET 303 '[JSON] CookieRedirect

type LoginBluesky =
  "start"
    :> Capture "handle" Bluesky.Handle
    :> Verb 'GET 303 '[JSON] CookieRedirect

type FacebookDecodeSignedRequest =
  ReqBody '[JSON] Facebook.SignedRequest
  :> Get '[JSON] Aeson.Value

type Api =
  "login" :> (
    Verb 'DELETE 204 '[JSON] (SetCookie NoContent)
    :<|> "facebook" :> LoginFacebook
    :<|> "google" :> LoginGoogle
    :<|> "friendica" :> LoginFriendica
    :<|> "bluesky" :> LoginBluesky
  )
  :<|> "facebook" :> "decode-signed-request" :> FacebookDecodeSignedRequest
  :<|> "bluesky"
    :> "client_metadata.json"
    :> Header "X-Forwarded-Host" Text
    :> Get '[JSON] Aeson.Value
