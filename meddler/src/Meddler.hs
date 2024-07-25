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
import qualified Data.UUID.Types as UUID
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Notification as SQL
import qualified Database.PostgreSQL.Simple.Types as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as QQ

data EmailRow = EmailRow
  { emailId :: Integer
  , rawRecipientAddresses :: SQL.PGArray Text
  , rawRecipientNames :: SQL.PGArray Text
  , rawWouldMatches :: Maybe (SQL.PGArray Text)
  , rawUnsubAddress :: Maybe Text
  , rawUnsubToken :: Maybe UUID.UUID
  } deriving (Show, Generic, SQL.FromRow)

data Email
  = Match
      { recipientAddresses :: [Text]
      , recipientNames :: [Text]
      , wouldMatches :: [Text]
      }
  | Unsub { unsubAddress :: Text, unsubToken :: UUID.UUID }

ofRow :: EmailRow -> Either [Text] Email
ofRow EmailRow
  { rawRecipientAddresses = SQL.PGArray recipientAddresses
  , rawRecipientNames = SQL.PGArray recipientNames
  , rawWouldMatches = Just (SQL.PGArray wouldMatches)
  , rawUnsubAddress = Nothing
  , rawUnsubToken = Nothing
  } = Right Match{ recipientAddresses, recipientNames, wouldMatches }
ofRow EmailRow
  { rawRecipientAddresses = SQL.PGArray []
  , rawRecipientNames = SQL.PGArray []
  , rawWouldMatches = Nothing
  , rawUnsubAddress = Just unsubAddress
  , rawUnsubToken = Just unsubToken
  } = Right Unsub{ unsubAddress, unsubToken }
ofRow other = Left ["Unexpected EmailRow: " <> Text.pack (show other)]

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
  toSend :: [EmailRow] <- SQL.query_ conn [QQ.sql|
      WITH from_queue AS (
        SELECT email_id
        FROM email_sending
        WHERE sending_started IS NULL
        AND sending_cancelled IS NULL
        ORDER BY unsub_contact_id IS NOT NULL DESC, created_at ASC
        LIMIT 1
        FOR UPDATE SKIP LOCKED
      ), to_send AS (
        UPDATE email_sending e
        SET sending_started = now()
        FROM from_queue q
        WHERE e.email_id = q.email_id
        RETURNING e.email_id, e.match_id, e.unsub_contact_id
      )
      SELECT
          t.email_id
        , array_remove(ARRAY[
            CASE WHEN NOT c1.blacklist THEN c1.email_address END
          , CASE WHEN NOT c2.blacklist THEN c2.email_address END
          ], NULL) AS recipient_addresses
        , array_remove(ARRAY[ma.name1, ma.name2], NULL) AS recipient_names
        , ma.would_matches
        , unsub.email_address AS unsub_address
        , unsub.unsub_token AS unsub_token
      FROM to_send t
      LEFT JOIN matches ma USING (match_id)
      LEFT JOIN contacts c1 ON (c1.contact_id = ma.contact_id1)
      LEFT JOIN contacts c2 ON (c2.contact_id = ma.contact_id2)
      LEFT JOIN contacts unsub ON (unsub.contact_id = t.unsub_contact_id)
    |]
  forM_ toSend $ \row@EmailRow{ emailId } -> do
    result <- case ofRow row of
      Left errors -> pure $ Left errors
      Right email -> f email
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
sendEmail aws email = do
  let
    sendEmailReq =
      SES.newSendEmail source destination message
      & SES.sendEmail_replyToAddresses .~ Just toAddresses
    source = "meddler@reciprocity.rpm.cc"
    toAddresses =
      case email of
        _ -> ["thebenmachine+ses@gmail.com"]
        Match{ recipientAddresses } -> recipientAddresses
        Unsub{ unsubAddress } -> [unsubAddress]
    destination =
      SES.newDestination
      & SES.destination_toAddresses .~ Just toAddresses
    message = SES.newMessage (SES.newContent subject) body
    subject =
      case email of
        Match{ recipientNames } ->
          "reciprocity match between " <> Text.intercalate " and " recipientNames
        Unsub{} -> "Unsubscribe from reciprocity emails"
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
      Text.unlines . concat $ case email of
        Match{ recipientNames, wouldMatches } ->
          [ [ ifHtml $ "<!DOCTYPE html>\n<html><head><title>" <> subject <> "</title></head><body>"
            , tag "p" "" $ "Hello " <> Text.intercalate " and " recipientNames <> ","
            , ""
            , ifHtml "<p>" <> "Good news! You have a shared enthusiasm for the following:" <> ifHtml "<ul>"
            ]
          , map (tag "li" "") wouldMatches
          , [ ifHtml "</ul>"
            , tag "p" "" "You can use this thread to organise if you want."
            , ""
            , tag "p" "" $
                "Love,\n" <> ifHtml "<br>"
                <> "The " <> tag "a" " href=\"https://reciprocity.rpm.cc\"" "reciprocity" <> " meddler bot"
            , ifHtml "</body></html>"
            ]
          ]
        Unsub{ unsubAddress, unsubToken } ->
          [ [ ifHtml $ "<!DOCTYPE html>\n<html><head><title>" <> subject <> "</title></head><body>"
            , tag "p" "" "Hello,"
            , ""
            , tag "p" "" $ "We've received a request to stop sending emails to " <> unsubAddress <> "."
            , ifHtml "</body></html>"
            ]
          ]
  response <- AWS.runResourceT $ AWS.send aws sendEmailReq
  let status = response ^. SES.sendEmailResponse_httpStatus
  pure $ if status == 200
    then Right ()
    else Left [Text.pack $ "sendEmail status " <> show status]
