module Facebook where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified System.Environment as Env

import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req

getAccessToken :: IO Text
getAccessToken = do
  appId <- Env.getEnv "FACEBOOK_APP_ID"
  appSecret <- Env.getEnv "FACEBOOK_APP_SECRET"
  pure . Text.pack $ appId <> "|" <> appSecret

-- I ended up not caring about the userid the user sent, but I'll keep this
-- distinction anyway for emphasis
data UserIdSource
  = FromClient
  | FromFacebook

newtype UserId (v :: UserIdSource) = UserId Text
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

newtype UserToken = UserToken { userToken :: Text }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON)

newtype DebugToken = DebugToken (UserId 'FromFacebook)

instance Aeson.FromJSON DebugToken where
  parseJSON = Aeson.withObject "DebugToken" $ \o ->
    Aeson.withObject "DebugToken.data" withData =<< o .: "data"
    where
      withData o = do
        True <- o .: "is_valid"
        DebugToken . UserId <$> o .: "user_id"

getUserId :: UserToken -> IO (UserId 'FromFacebook)
getUserId (UserToken userToken) = do
  accessToken <- getAccessToken
  resp <-
    Req.runReq Req.defaultHttpConfig
    $ Req.req
      Req.GET
      (Req.https "graph.facebook.com" /: "debug_token")
      Req.NoReqBody
      Req.jsonResponse
    $ mconcat
        [ "input_token" =: userToken
        , "access_token" =: accessToken
        ]
  let DebugToken verifiedUserId = Req.responseBody resp
  pure verifiedUserId
