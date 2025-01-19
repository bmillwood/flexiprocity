{-# LANGUAGE QuasiQuotes #-}
module Agent where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor as Bi
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import System.IO

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.FromField as SQL
import qualified Database.PostgreSQL.Simple.Notification as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as QQ
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI as URI
import qualified Network.URI.Static as URI

import qualified Bluesky.Did as Did
import qualified Bluesky.Handle as Handle

data Task
  = BskyProfile Did.Did
  | BskyMutuals Did.Did
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | https://docs.bsky.app/docs/api/app-bsky-graph-get-profile
data Profile = Profile
  { did :: Did.Did
  , handle :: Handle.Handle
  , displayName :: Text
  , avatar :: Text
  , followersCount :: Integer
  , followsCount :: Integer
  } deriving stock (Show, Generic)
    deriving anyclass (Aeson.FromJSON)

apiBase :: URI.URI
apiBase = [URI.uri|https://public.api.bsky.app|]

bskyProfileTask :: HTTP.Manager -> SQL.Connection -> Did.Did -> IO ()
bskyProfileTask httpManager conn taskDid = do
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
  affected :: [SQL.Only Integer] <- SQL.query conn [QQ.sql|
      WITH upd(did, name, picture) AS (
        VALUES (?, ?, ?)
      )
      UPDATE users u
      SET name = upd.name, picture = upd.picture
      FROM upd, bluesky_login bs
      WHERE u.user_id = bs.user_id
      AND bs.bluesky_did = upd.did
      AND (u.name <> upd.name OR u.picture <> upd.picture)
      RETURNING u.user_id
    |] (Did.rawDid did, displayName, avatar)
  putStrLn $ "Updated " <> show (length affected) <> " profile(s)"

mapToChunks :: (Ord k) => Int -> Map k v -> [Map k v]
mapToChunks size = unfoldr go
  where
    go m
      | Map.null m = Nothing
      | otherwise = Just $ Map.splitAt size m

newtype PgDid = PgDid { unPgDid :: Did.Did }

instance SQL.FromField PgDid where
  fromField f bs =
    Did.makeDid <$> SQL.fromField f bs
    >>= \case
      Left err -> SQL.returnError SQL.ConversionFailed f (show err)
      Right did -> pure (PgDid did)

data Relationship = Relationship
  { theirDid :: Did.Did
  , follows :: Bool
  , isFollowedBy :: Bool
  } deriving stock (Show, Generic)

instance Aeson.FromJSON Relationship where
  parseJSON = Aeson.withObject "Relationship" $ \o -> do
    theirDid <- o .: "did"
    let has k = isJust <$> (o .:? k :: Aeson.Parser (Maybe Aeson.Value))
    follows <- has "following"
    isFollowedBy <- has "followedBy"
    pure Relationship{ theirDid, follows, isFollowedBy }

data Relationships = Relationships
  { actor :: Did.Did
  , relationships :: [Relationship]
  } deriving stock (Show, Generic)
    deriving anyclass (Aeson.FromJSON)

-- We have (at least) three ways to learn about follower relationships:
-- app.bsky.actor.getFollows
-- app.bsky.actor.getFollowers
-- app.bsky.graph.getRelationships
-- We probably want to pick based on whichever is smallest out of:
-- user profile followCount
-- user profile followerCount
-- total flexiprocity users on Bluesky
-- For now we'll just assume the last of these three :)
bskyMutualsTask :: HTTP.Manager -> SQL.Connection -> Did.Did -> IO ()
bskyMutualsTask httpManager conn taskDid = do
  didUserMap :: Map Did.Did Integer
    <- Map.fromList . map (Bi.first unPgDid)
      <$> SQL.query_ conn [QQ.sql| SELECT bluesky_did, user_id FROM bluesky_login |]
  taskUserId <- maybe
    (fail "Can't find taskDid in bluesky_login")
    pure
    (Map.lookup taskDid didUserMap)
  let
    didUserMaps = mapToChunks 30 $ Map.delete taskDid didUserMap
  putStrLn $ "Making " <> show (length didUserMaps) <> " API calls for mutuals"
  mutualUserIds <- fmap Set.unions . forM didUserMaps $ \dum -> do
    let
      uriQuery = concat
        $ "?actor=" <> Text.unpack (Did.rawDid taskDid)
        : map (("&others=" <>) . Text.unpack . Did.rawDid) (Map.keys dum)
    req <- HTTP.requestFromURI $ apiBase
      { URI.uriPath = "/xrpc/app.bsky.graph.getRelationships"
      , URI.uriQuery
      }
    resp <- HTTP.httpLbs req httpManager
    Relationships{ actor, relationships }
      <- either fail pure . Aeson.eitherDecode $ HTTP.responseBody resp
    when (actor /= taskDid) . error
      $ "returned did not match query: " <> show (actor, taskDid)
    let
      isMutual Relationship{ follows, isFollowedBy } = follows && isFollowedBy
      mutuals = map theirDid $ filter isMutual relationships
    pure . Set.fromList $ mapMaybe (\d -> Map.lookup d dum) mutuals
  currentMutualUserIds <- SQL.query conn [QQ.sql|
      SELECT friend_id FROM bluesky_mutuals WHERE user_id = ?
      UNION ALL
      SELECT user_id FROM bluesky_mutuals WHERE friend_id = ?
    |] (taskUserId, taskUserId)
       <&> Set.fromList . map (\(SQL.Only uid) -> uid)
  let
    newMutuals = Set.difference mutualUserIds currentMutualUserIds
    noLonger = Set.difference currentMutualUserIds mutualUserIds
    pairWithMe them =
      -- allow me my little indulgences
      liftA2 (liftA2 (,)) min max them taskUserId
  unless (Set.null newMutuals) $ do
    count <- SQL.executeMany conn [QQ.sql|
        INSERT INTO bluesky_mutuals (user_id, friend_id) VALUES (?,?)
      |] $ map pairWithMe (Set.toList newMutuals)
    putStrLn $ "Added " <> show count <> " mutuals"
  unless (Set.null noLonger) $ do
    count <- SQL.executeMany conn [QQ.sql|
        DELETE FROM bluesky_mutuals
        WHERE user_id = ? AND friend_id = ?
      |] $ map pairWithMe (Set.toList noLonger)
    putStrLn $ "Removed " <> show count <> " mutuals"

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
      BskyProfile taskDid -> bskyProfileTask httpManager conn taskDid
      BskyMutuals taskDid -> bskyMutualsTask httpManager conn taskDid
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
