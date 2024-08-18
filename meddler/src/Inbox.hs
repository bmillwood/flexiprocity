module Inbox where

import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import System.Environment
import System.IO

import qualified Amazonka as AWS
import qualified Amazonka.SQS as SQS
import qualified Amazonka.SQS.ReceiveMessage as SQS
import qualified Amazonka.SQS.Types.Message as SQS
import qualified Data.Aeson as Aeson

import qualified StructuralAeson as StAe

data SQSBody = SQSBody
  { messageId :: Text
  , topicArn :: Text
  , subject :: Text
  , message :: StAe.Embedded (StAe.At "content" Text)
  } deriving stock (Generic, Show)

initialUpper :: String -> String
initialUpper "" = ""
initialUpper (c : cs) = toUpper c : cs

instance Aeson.FromJSON SQSBody where
  parseJSON =
    Aeson.genericParseJSON
      Aeson.defaultOptions{ Aeson.fieldLabelModifier = initialUpper }

deleteQueueMessage :: AWS.Env -> Text -> SQS.Message -> IO ()
deleteQueueMessage aws queueUrl msg =
  case msg ^. SQS.message_receiptHandle of
    Nothing -> error "no receipt handle?"
    Just handle -> do
      SQS.DeleteMessageResponse' <- AWS.runResourceT
        $ AWS.send aws
        $ SQS.newDeleteMessage queueUrl handle
      putStrLn $ "deleted messageId " <> show (msg ^. SQS.message_messageId)

inbox :: AWS.Env -> IO ()
inbox aws = forever $ do
  threadDelay 1_000_000
  queueUrl <- Text.pack <$> getEnv "QUEUE_URL"
  let
    receiveMessage =
      SQS.newReceiveMessage queueUrl
      & SQS.receiveMessage_waitTimeSeconds .~ Just 20
  receiveResp <- AWS.runResourceT $ AWS.send aws receiveMessage
  when
    (receiveResp ^. SQS.receiveMessageResponse_httpStatus /= 200)
    (error ("receiveMessage status code check failed: " <> show receiveResp))
  case receiveResp ^. SQS.receiveMessageResponse_messages of
    Nothing -> putStr "." >> hFlush stdout
    Just msgs ->
      forM_ msgs $ \msg -> do
        case msg ^. SQS.message_body of
          Nothing -> error $ "no body? " <> show msg
          Just body ->
            case Aeson.eitherDecode $ BSL.fromStrict $ Text.encodeUtf8 body of
              Left err -> error $ "decoding body: " <> err
              Right SQSBody{ message = StAe.Embedded (StAe.At email) } ->
                print email
        --deleteQueueMessage aws queueUrl msg
