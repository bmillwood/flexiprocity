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
import qualified Control.Concurrent.TokenBucket as TokenBucket
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
  rateLimit <- createRateLimiter
  conn <- SQL.connectPostgreSQL "user=meddler dbname=flexiprocity"
  _ <- SQL.execute_ conn "LISTEN email_sending"
  notified <- newEmptyMVar
  withAsync (notifyThread conn notified) $ \_ -> forever $ do
    putStrLn "checking for emails to send"
    withEmails conn $ \email -> do
      rateLimit
      putStrLn "sending an email"
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

createRateLimiter :: IO (IO ())
createRateLimiter = do
  rateLimiter <- TokenBucket.newTokenBucket
  let
    burstSize = 10
    secondsPerEmail = 300
    usPerEmail = secondsPerEmail * 1_000_000
  -- fill the token bucket, so we can't exceed rate limits by restarting
  -- True <- TokenBucket.tokenBucketTryAlloc rateLimiter burstSize usPerEmail burstSize
  pure $ TokenBucket.tokenBucketWait rateLimiter burstSize usPerEmail

withEmails :: SQL.Connection -> (Email -> IO (Either [Text] ())) -> IO ()
withEmails conn f = do
  toSend :: [Email] <- SQL.query_ conn [QQ.sql|
      WITH from_queue AS (
        SELECT email_id
        FROM email_sending
        WHERE sending_started IS NULL
        AND sending_cancelled IS NULL
        AND match_id IS NOT NULL
        ORDER BY created_at ASC
        LIMIT 1
        FOR UPDATE SKIP LOCKED
      ), to_send AS (
        UPDATE email_sending e SET sending_started = now()
        FROM from_queue q
        WHERE e.email_id = q.email_id
        RETURNING e.email_id, e.match_id
      )
      SELECT
          t.email_id
        , array_remove(ARRAY[
            CASE WHEN NOT c1.blacklist THEN c1.email_address END
          , CASE WHEN NOT c2.blacklist THEN c2.email_address END
          ], NULL) AS recipient_addresses
        , ARRAY[ma.name1, ma.name2] AS recipient_names
        , ma.would_matches
      FROM to_send t
      LEFT JOIN matches ma USING (match_id)
      LEFT JOIN contacts c1 ON (c1.contact_id = ma.contact_id1)
      LEFT JOIN contacts c2 ON (c2.contact_id = ma.contact_id2)
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
      else ["thebenmachine+ses@gmail.com"]
    destination =
      SES.newDestination
      & SES.destination_toAddresses .~ Just toAddresses
    message = SES.newMessage (SES.newContent subject) body
    subject = "reciprocity match between " <> Text.intercalate " and " names
    names = SQL.fromPGArray recipientNames
    body =
      SES.newBody
      & SES.body_text .~ Just (SES.newContent (mkBody False))
      & SES.body_html .~ Just (SES.newContent (mkBody True))
    mkBody isHtml =
      let
        ifHtml s = if isHtml then s else ""
        tag n onOpen content =
          (ifHtml $ "<" <> n <> onOpen <> ">") <> content <> (ifHtml $ "</" <> n <> ">")
      in
      Text.unlines $ concat
        [ [ ifHtml $ "<!DOCTYPE html>\n<html><head><title>" <> subject <> "</title></head><body>"
          , tag "p" "" $ "Hello " <> Text.intercalate " and " names <> ","
          , ""
          , ifHtml "<p>" <> "Good news! You have a shared enthusiasm for the following:" <> ifHtml "<ul>"
          ]
        , map (tag "li" "") (SQL.fromPGArray wouldMatches)
        , [ ifHtml "</ul>"
          , tag "p" "" "You can use this thread to organise if you want."
          , ""
          , tag "p" "" $
               "Love,\n" <> ifHtml "<br>"
               <> "The " <> tag "a" " href=\"https://reciprocity.rpm.cc\"" "reciprocity" <> " meddler bot"
          , ifHtml "</body></html>"
          ]
        ]
  response <- AWS.runResourceT $ AWS.send aws sendEmailReq
  let status = response ^. SES.sendEmailResponse_httpStatus
  pure $ if status == 200
    then Right ()
    else Left [Text.pack $ "sendEmail status " <> show status]
