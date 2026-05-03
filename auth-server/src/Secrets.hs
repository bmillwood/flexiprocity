module Secrets where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text.Encoding as Text
import qualified System.Environment as Env

getDir :: IO FilePath
getDir = fromMaybe "../secrets" <$> Env.lookupEnv "SECRETS_DIR"

getJson :: (Aeson.FromJSON a) => FilePath -> IO a
getJson path = do
  secretsDir <- getDir
  secret <-
    either fail pure
      =<< Aeson.eitherDecode <$> BSL.readFile (secretsDir <> "/" <> path)
  pure secret

data ClientSecret = ClientSecret
  { clientId :: BS.ByteString
  , clientSecret :: BS.ByteString
  }

newtype GoogleSecret = GoogleSecret ClientSecret

instance Aeson.FromJSON GoogleSecret where
  parseJSON = Aeson.withObject "ClientSecret" $ \o -> do
    web <- o .: "web"
    clientId <- Text.encodeUtf8 <$> web .: "client_id"
    clientSecret <- Text.encodeUtf8 <$> web .: "client_secret"
    pure $ GoogleSecret ClientSecret{ clientId, clientSecret }
