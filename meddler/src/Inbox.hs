module Inbox where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Char
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import System.Environment
import System.IO

import qualified Amazonka as AWS
import qualified Amazonka.SQS as SQS
import qualified Amazonka.SQS.ReceiveMessage as SQS
import qualified Amazonka.SQS.Types.Message as SQS
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AP
import qualified Data.IMF as IMF
import qualified Data.MIME as MIME
import qualified Email.DSN.DeliveryStatus as DS
import qualified Email.DSN.StatusCode as DSNCode

import qualified StructuralAeson as StAe
import qualified Task

newtype SQSMessage = SQSMessage Text
  deriving Aeson.FromJSON via (StAe.Embedded (StAe.At "content" Text))
  deriving stock (Show)

data SQSBody = SQSBody
  { messageId :: Text
  , topicArn :: Text
  , subject :: Text
  , message :: SQSMessage
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

parseFailingAddressesFromBounce :: BS.ByteString -> Either String [Text]
parseFailingAddressesFromBounce email = do
  IMF.Message _ body <- IMF.parse (IMF.message MIME.mime) email
  -- I think maybe msgs should be decoded somehow but I'm not gonna do it
  msgs <- case body of
    MIME.Multipart (MIME.Report "delivery-status") _ msgs -> Right msgs
    _ -> Left "probably not a bounce?"
  DS.DeliveryStatus { perRecipientFields } <- case getFirst $ foldMap (First . getStatus) msgs of
    Nothing -> Left "couldn't find delivery status"
    Just part -> AP.parseOnly DS.parser (Text.decodeUtf8 part)
  Right $ mapMaybe emailFromRecipientFields (NE.toList perRecipientFields)
  where
    getStatus (IMF.Message headers body) =
      case headers ^. MIME.contentType of
        MIME.ContentType "message" "delivery-status" _ ->
          case body of
            MIME.Part bs -> Just bs
            _ -> error "Unexpected non-Part in message/delivery-status"
        _ -> Nothing
    emailFromRecipientFields :: NE.NonEmpty DS.PerRecipientField -> Maybe Text
    emailFromRecipientFields = join . foldr searchFields (Just Nothing)
    searchFields DS.OtherPerRecipient{} z = z
    searchFields DS.FinalRecipient{ address } z = Just address <$ z
    searchFields (DS.Status (DSNCode.StatusCode cl _)) z
      | cl == DSNCode.PermanentFailure = z
      | otherwise = Nothing

handleEmail :: Chan Task.Task -> Text -> IO ()
handleEmail tasks email =
  case parseFailingAddressesFromBounce (Text.encodeUtf8 email) of
    Left err -> putStrLn $ "handleEmail: " <> err
    Right addresses -> writeChan tasks (Task.DisableEmails addresses)

inbox :: AWS.Env -> Chan Task.Task -> IO ()
inbox aws tasks = forever $ do
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
    Just [] -> putStr "_" >> hFlush stdout
    Just msgs ->
      forM_ msgs $ \msg -> do
        case msg ^. SQS.message_body of
          Nothing -> error $ "no body? " <> show msg
          Just body ->
            case Aeson.eitherDecode $ BSL.fromStrict $ Text.encodeUtf8 body of
              Left err -> error $ "decoding body: " <> err
              Right SQSBody{ message = SQSMessage email } ->
                handleEmail tasks email
        deleteQueueMessage aws queueUrl msg
