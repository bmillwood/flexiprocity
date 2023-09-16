module Facebook where

import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Encoding as Enc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import qualified System.Environment as Env

import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req

getAppSecret :: IO String
getAppSecret = Env.getEnv "FACEBOOK_APP_SECRET"

getAccessToken :: IO Text
getAccessToken = do
  appId <- Env.getEnv "FACEBOOK_APP_ID"
  appSecret <- getAppSecret
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

newtype SignedRequest = SignedRequest { signedRequest :: Text }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON)

decodeSignedRequestWithSecret
  :: SignedRequest -> BS.ByteString -> Either String Aeson.Value
decodeSignedRequestWithSecret SignedRequest{ signedRequest } appSecret =
  case BSC.split '.' (Text.encodeUtf8 signedRequest) of
    [encSig, encPayload] -> do
      decodedSig <- Enc.convertFromBase Enc.Base64 encSig >>= hmacFromBS
      decodedPayload <- Enc.convertFromBase Enc.Base64 encPayload
      let computedSig = HMAC.hmac appSecret decodedPayload
      () <-
        if computedSig == decodedSig
        then Right ()
        else Left "sig does not match"
      Aeson.eitherDecode (BSL.fromStrict decodedPayload)
    other -> Left $ "splitting by . gave " <> show (length other) <> " pieces"
  where
    hmacFromBS :: BS.ByteString -> Either String (HMAC.HMAC Hash.SHA256)
    hmacFromBS =
      maybe
        (Left "digestFromByteString failed")
        (Right . HMAC.HMAC)
      . Hash.digestFromByteString

decodeSignedRequest :: SignedRequest -> IO (Either String Aeson.Value)
decodeSignedRequest signedReq = do
  print ("decodeSignedRequest"::Text, signedReq)
  decodeSignedRequestWithSecret signedReq . BSC.pack <$> getAppSecret
