module Secrets where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified System.Environment as Env

getDir :: IO FilePath
getDir = fromMaybe "../secrets" <$> Env.lookupEnv "SECRETS_DIR"

getJson :: (Aeson.FromJSON a) => FilePath -> IO a
getJson path = do
  secretsDir <- getDir
  Right secret <- Aeson.eitherDecode <$> BSL.readFile (secretsDir <> "/" <> path)
  pure secret
