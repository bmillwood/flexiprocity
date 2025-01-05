{-# LANGUAGE ViewPatterns #-}
module Bluesky where

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics

import qualified Network.HTTP.Client as HTTP
import qualified Servant

import qualified BlueskyApi
import qualified StructuralAeson

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

newtype ResolveHandle = ResolveHandle { did :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.FromJSON)

handleDID :: Env -> BlueskyApi.Handle -> IO Text
handleDID Env{ httpManager } (BlueskyApi.getHandle -> rawHandle) = do
  let rawHandleString = Text.unpack rawHandle
  req <-
    HTTP.parseUrlThrow
      -- This could be a bad thing to do if the handle was arbitrary user
      -- data. But we validated it on the way in.
      -- (It still might be better to construct the URL in some structured way
      -- that insists the handle can only go in the hostname portion. But this
      -- will do.)
      ("https://" <> rawHandleString <> "/xrpc/com.atproto.identity.resolveHandle?handle=" <> rawHandleString)
  resp <- HTTP.httpLbs req httpManager
  StructuralAeson.Field @"did" did <- either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp
  pure did

-- https://github.com/did-method-plc/did-method-plc
-- only appropriate on plc DIDs, but those are all we're using for now
didDocument :: Env -> Text -> IO Aeson.Value
didDocument Env{ httpManager } did = do
  req <- HTTP.parseUrlThrow ("https://plc.directory/" <> Text.unpack did)
  resp <- HTTP.httpLbs req httpManager
  either fail pure $ Aeson.eitherDecode $ HTTP.responseBody resp

start :: Env -> BlueskyApi.Handle -> Servant.Handler Aeson.Value
start env handle = do
  liftIO $ didDocument env =<< handleDID env handle
