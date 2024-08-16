module Inbox where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (fix)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

newtype SESEvents = SESEvents [SESEvent] deriving stock (Show, Generic)
data SESEvent = SESEvent
  { mail :: Aeson.Value
  , receipt :: Aeson.Value
  } deriving stock (Show)

instance Aeson.FromJSON SESEvents where
  parseJSON = Aeson.withObject "SESEvents" $ \o -> do
    SESEvents <$> o .: "Records"

instance Aeson.FromJSON SESEvent where
  parseJSON = Aeson.withObject "SESEvent" $ \o -> do
    "aws:ses" :: Text <- o .: "eventSource"
    "1.0" :: Text <- o .: "eventVersion"
    ses <- o .: "ses"
    mail <- ses .: "mail"
    receipt <- ses .: "receipt"
    pure SESEvent{ mail, receipt }

inbox :: IO ()
inbox = Warp.runEnv 57960 $ \req respond ->
  if Wai.rawPathInfo req /= "/inbox"
  then respond (Wai.responseLBS HTTP.notFound404 [] "Not found :(")
  else if Wai.requestMethod req /= "POST"
  then respond (Wai.responseLBS HTTP.methodNotAllowed405 [] "Not POST :(")
  else do
    print req
    body <- BSL.fromChunks <$> fix \again -> do
      chunk <- Wai.getRequestBodyChunk req
      if BS.null chunk then pure [] else (chunk :) <$> again
    print body
    print (Aeson.eitherDecode @SESEvents body)
    respond (Wai.responseLBS HTTP.ok200 [] "ok!")
