{-# LANGUAGE ViewPatterns #-}
module Bluesky where

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Bluesky.Handle as Bsky
import qualified Network.HTTP.Client as HTTP
import qualified Servant

data Env = Env
  { httpManager :: HTTP.Manager }

init :: HTTP.Manager -> IO Env
init httpManager = pure $ Env{ httpManager }

-- | https://atproto.com/specs/oauth#clients
clientMetadata :: Text -> Aeson.Value
clientMetadata host = Aeson.object
  [ "client_id" .= (base_uri <> "/bluesky/client_metadata.json")
  , "application_type" .= ("web" :: Text)
  , "grant_types" .= ["authorization_code" :: Text]
  , "scope" .= ("atproto" :: Text)
  , "response_types" .= ["code" :: Text]
  , "redirect_uris" .= [base_uri <> "/login/bluesky/complete"]
  , "dpop_bound_access_tokens" .= True
  , "client_name" .= ("flexiprocity" :: Text)
  , "client_uri" .= ("https://" <> host)
  ]
  where
    base_uri = "https://" <> host <> "/auth"

serveClientMetadata :: Maybe Text -> Servant.Handler Aeson.Value
serveClientMetadata Nothing =
  Except.throwError Servant.err400{ Servant.errBody = "Missing Host header" }
serveClientMetadata (Just host) =
  pure (clientMetadata host)

start :: Env -> Bsky.Handle -> Servant.Handler Aeson.Value
start Env{ httpManager } handle = do
  did <- liftIO $ Bsky.resolveVerify httpManager handle
  pure (Aeson.String (Text.pack (show did)))
