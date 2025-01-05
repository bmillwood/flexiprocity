module Bluesky where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)

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
