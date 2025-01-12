{-# LANGUAGE QuasiQuotes #-}
module Agent where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import System.IO

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Notification as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as QQ
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI as URI
import qualified Network.URI.Static as URI

import qualified Bluesky.Did as Did

data Task
  = BskyProfile Did.Did
  | BskyMutuals Did.Did
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | https://docs.bsky.app/docs/api/app-bsky-graph-get-profile
data Profile = Profile
  { did :: Did.Did
  , displayName :: Text
  , avatar :: Text
  } deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

apiBase :: URI.URI
apiBase = [URI.uri|https://public.api.bsky.app|]

bskyProfile :: HTTP.Manager -> SQL.Connection -> Did.Did -> IO ()
bskyProfile httpManager conn taskDid = do
  req <- HTTP.requestFromURI $ apiBase
    { URI.uriPath = "/xrpc/app.bsky.actor.getProfile"
    , -- Not sure whether I should escape actor or not. If I'm wrong then at
      -- worst it should fail when we check the returned DID.
      URI.uriQuery = "?actor=" <> Text.unpack (Did.rawDid taskDid)
    }
  resp <- HTTP.httpLbs req httpManager
  Profile{ did, displayName, avatar }
    <- either fail pure . Aeson.eitherDecode $ HTTP.responseBody resp
  when (did /= taskDid) . error
    $ "returned did not match query: " <> show (did, taskDid)
  affected <- SQL.execute conn [QQ.sql|
      UPDATE users u
      SET name = ?, picture = ?
      FROM bluesky_login bs
      WHERE u.user_id = bs.user_id
      AND bs.bluesky_did = ?
    |] (displayName, avatar, Did.rawDid did)
  putStrLn $ "Updated " <> show affected <> " profile(s)"

bskyMutuals :: HTTP.Manager -> SQL.Connection -> Did.Did -> IO ()
bskyMutuals httpManager conn taskDid = do
  req <- HTTP.requestFromURI $ apiBase
    { URI.uriPath = "/xrpc/app.bsky.actor.getFollows"
    , -- see above re: escaping actor
      URI.uriQuery = "?actor=" <> Text.unpack (Did.rawDid taskDid)
    }
  resp <- HTTP.httpLbs req httpManager
  print resp

fetchAndDoTasks :: HTTP.Manager -> SQL.Connection -> IO ()
fetchAndDoTasks httpManager conn = do
  done <- SQL.withTransaction conn $ do
    tasks <-
      mapM (\(SQL.Only v) -> either fail pure $ Aeson.parseEither Aeson.parseJSON v)
      =<< SQL.query_ conn [QQ.sql|
        WITH t AS (
          SELECT id FROM agent_tasks ORDER BY requested_at ASC LIMIT 1
          FOR UPDATE SKIP LOCKED
        )
        DELETE FROM agent_tasks todo
        USING t WHERE todo.id = t.id
        RETURNING todo.task
      |]
    for_ tasks $ \case
      BskyProfile taskDid -> bskyProfile httpManager conn taskDid
      BskyMutuals taskDid -> bskyMutuals httpManager conn taskDid
    pure (null tasks)
  when (not done) $ do
    threadDelay 1000000
    fetchAndDoTasks httpManager conn

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  httpManager <- HTTP.newManager HTTPS.tlsManagerSettings
  wakeUp <- newChan
  writeChan wakeUp ()
  conn <- SQL.connectPostgreSQL "user=agent dbname=flexiprocity"
  _ <- SQL.execute_ conn "LISTEN agent"
  let
    notifyThread = forever $ do
      SQL.Notification{} <- SQL.getNotification conn
      putStrLn "Received notification"
      writeChan wakeUp ()
  withLinkedAsync notifyThread . forever $ do
    () <- readChan wakeUp
    fetchAndDoTasks httpManager conn
  where
    withLinkedAsync act then_ = withAsync act $ \thread -> link thread >> then_
