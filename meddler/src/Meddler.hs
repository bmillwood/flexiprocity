{-# LANGUAGE QuasiQuotes #-}
module Meddler (main) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import qualified Amazonka as AWS
import qualified Amazonka.SES as SES
import qualified Amazonka.SES.SendEmail as SES
import qualified Amazonka.SES.Types as SES
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Notification as SQL
import qualified Database.PostgreSQL.Simple.Types as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as QQ

data Email = Email
  { emailId :: Integer
  , recipientAddresses :: SQL.PGArray Text
  , recipientNames :: SQL.PGArray Text
  , wouldMatches :: SQL.PGArray Text
  } deriving (Show, Generic, SQL.FromRow)

main :: IO ()
main = do
  aws <- AWS.newEnv AWS.discover
  conn <- SQL.connectPostgreSQL "user=meddler dbname=flexiprocity"
  _ <- SQL.execute_ conn "LISTEN email_sending"
  notified <- newEmptyMVar
  withAsync (notifyThread conn notified) $ \_ -> forever $ do
    putStrLn "checking for emails to send"
    withEmails conn $ \email -> do
      sendEmail aws email `catch` \(err :: IOException) ->
        pure (Left [Text.pack $ show err])
    putStrLn "done sending emails, going to sleep"
    takeMVar notified
  where
    notifyThread conn notified = forever $ do
      n@SQL.Notification{ notificationChannel } <- SQL.getNotification conn
      putStrLn $ "notification received: " <> show n
      when (notificationChannel == "email_sending") $ do
        putStrLn "wake up main thread"
        (_ :: Bool) <- tryPutMVar notified ()
        pure ()

withEmails :: SQL.Connection -> (Email -> IO (Either [Text] ())) -> IO ()
withEmails conn f = do
  toSend :: [Email] <- SQL.query_ conn [QQ.sql|
      WITH to_send AS (
        SELECT email_id
        FROM email_sending
        WHERE sending_started IS NULL
        AND sending_cancelled IS NULL
        ORDER BY created_at ASC
        LIMIT 1
        FOR UPDATE SKIP LOCKED
      )
      UPDATE email_sending e SET sending_started = now()
      FROM to_send
      WHERE e.email_id = to_send.email_id
      RETURNING e.email_id, recipient_addresses, recipient_names, would_matches
    |]
  forM_ toSend $ \email@Email{ emailId } -> do
    result <- f email
    case result of
      Left errors ->
        SQL.execute conn
          [QQ.sql| UPDATE email_sending
            SET sending_cancelled = now(), errors = ?
            WHERE email_id = ?
          |]
          (SQL.PGArray errors, emailId)
      Right () ->
        SQL.execute conn
          [QQ.sql| UPDATE email_sending
            SET sending_completed = now()
            WHERE email_id = ?
          |]
          (SQL.Only emailId)
  unless (null toSend) $ withEmails conn f

sendEmail :: AWS.Env -> Email -> IO (Either [Text] ())
sendEmail aws Email{ recipientAddresses, recipientNames, wouldMatches } = do
  let
    sendEmailReq =
      SES.newSendEmail source destination message
      & SES.sendEmail_replyToAddresses .~ Just toAddresses
    source = "meddler@reciprocity.rpm.cc"
    toAddresses =
      if False
      then SQL.fromPGArray recipientAddresses
      else ["success@simulator.amazonses.com"]
    destination =
      SES.newDestination
      & SES.destination_toAddresses .~ Just toAddresses
    message = SES.newMessage content body
    names = SQL.fromPGArray recipientNames
    content = SES.newContent
      $ "reciprocity match between "
      <> Text.intercalate " and " names
    bodyText =
      Text.unlines $ concat
        [ [ "Hello " <> Text.intercalate " and " names <> ","
          , ""
          , "Good news! You have a shared enthusiasm for:"
          ]
        , map ("- " <>) (SQL.fromPGArray wouldMatches)
        , [ ""
          , "You can use this thread to organise if you want."
          , ""
          , "Love,"
          , "The reciprocity meddler bot"
          ]
        ]
    body =
      SES.newBody
      & SES.body_text .~ Just (SES.newContent bodyText)
  response <- AWS.runResourceT $ AWS.send aws sendEmailReq
  let status = response ^. SES.sendEmailResponse_httpStatus
  pure $ if status == 200
    then Right ()
    else Left [Text.pack $ "sendEmail status " <> show status]
