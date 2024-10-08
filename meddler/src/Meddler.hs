{-# LANGUAGE QuasiQuotes #-}
module Meddler (main) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO

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

import qualified Inbox
import qualified Task

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
  hSetBuffering stdout LineBuffering
  aws <- AWS.newEnv AWS.discover
  tasks <- newChan
  writeChan tasks Task.SendEmails
  withAsync (Inbox.inbox aws tasks) $ \inbox -> link inbox >> do
    rateLimit <- createRateLimiter
    conn <- SQL.connectPostgreSQL "user=meddler dbname=flexiprocity"
    _ <- SQL.execute_ conn "LISTEN email_sending"
    withAsync (notifyThread conn tasks) $ \_ -> forever $ do
      task <- readChan tasks
      case task of
        Task.SendEmails -> do
          putStrLn "checking for emails to send"
          withEmails conn $ \email -> do
            rateLimit
            putStrLn "sending an email"
            sendEmail aws email `catch` \(err :: IOException) ->
              pure (Left [Text.pack $ show err])
          putStrLn "done sending emails"
        Task.DisableEmails addresses -> do
          putStrLn $ "disabling e-mail addresses: " <> show addresses
          results :: [SQL.Only (Maybe Text)] <- SQL.returning conn [QQ.sql|
              SELECT email_bounced(e)::text
              FROM (VALUES (?)) es(e)
            |]
            (map SQL.Only addresses)
          putStrLn
            $ "disabled "
            <> show (length (filter (\(SQL.Only mt) -> isJust mt) results))
            <> " of "
            <> show (length addresses)
  where
    notifyThread conn tasks = forever $ do
      n@SQL.Notification{ notificationChannel } <- SQL.getNotification conn
      putStrLn $ "notification received: " <> show n
      when (notificationChannel == "email_sending") $ do
        putStrLn "wake up main thread"
        writeChan tasks Task.SendEmails
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

withEmails :: SQL.Connection -> (Email -> IO (Either [Text] Text)) -> IO ()
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
            CASE WHEN c1.disable_reason IS NULL THEN c1.email_address END
          , CASE WHEN c2.disable_reason IS NULL THEN c2.email_address END
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
      Right messageId ->
        SQL.execute conn
          [QQ.sql| UPDATE email_sending
            SET sending_completed = now()
              , message_id = ?
            WHERE email_id = ?
          |]
          (messageId, emailId)
  unless (null toSend) $ withEmails conn f

sendEmail :: AWS.Env -> Email -> IO (Either [Text] Text)
sendEmail aws email = do
  recipientOverride <- lookupEnv "RECIPIENT_OVERRIDE"
  let
    sendEmailReq =
      SES.newSendEmail source destination message
      & SES.sendEmail_replyToAddresses .~ Just toAddresses
    source = "meddler@reciprocity.rpm.cc"
    toAddresses =
      case (recipientOverride, email) of
        (Just override, _) -> [Text.pack override]
        (Nothing, Match{ recipientAddresses }) -> recipientAddresses
        (Nothing, Unsub{ unsubAddress }) -> [unsubAddress]
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
        reciprocityBaseUrl = "https://reciprocity.rpm.cc"
        unsubBaseUrl = reciprocityBaseUrl <> "/unsubscribe"
        linkTo dest = tag "a" (" href=\"" <> dest <> "\"")
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
                <> "The " <> linkTo reciprocityBaseUrl "reciprocity" <> " meddler bot"
            , ""
            , tag "p" " style=\"font-size: 80%\"" $ Text.concat
                [ linkTo unsubBaseUrl "Stop receiving these emails"
                , if not isHtml
                  then ": " <> unsubBaseUrl
                  else ""
                ]
            , ifHtml "</body></html>"
            ]
          ]
        Unsub{ unsubAddress, unsubToken } ->
          let
            unsubUrl =
              unsubBaseUrl
              <> "?address=" <> unsubAddress
              <> "&token=" <> UUID.toText unsubToken
          in
          [ [ ifHtml $ "<!DOCTYPE html>\n<html><head><title>" <> subject <> "</title></head><body>"
            , tag "p" "" "Hello,"
            , ""
            , tag "p" "" $ "We've received a request to stop sending emails to " <> unsubAddress <> "."
            , ""
            , tag "p" "" $ Text.concat
                [ "The recommended way to achieve this is just by updating your account settings,"
                , " but just in case that's ever not sufficient (e.g. you can't log in), this"
                , " e-mail offers another mechanism to permanently ban reciprocity from e-mailing you."
                ]
            , ""
            , tag "p" "" $ Text.unlines
                [ "Please visit the following URL to complete the process:" <> ifHtml "<br>"
                , linkTo unsubUrl unsubUrl
                ]
            , ifHtml "</body></html>"
            ]
          ]
  response <- AWS.runResourceT $ AWS.send aws sendEmailReq
  let status = response ^. SES.sendEmailResponse_httpStatus
  pure $ if status == 200
    then Right $ response ^. SES.sendEmailResponse_messageId
    else Left [Text.pack $ "sendEmail status " <> show status]
