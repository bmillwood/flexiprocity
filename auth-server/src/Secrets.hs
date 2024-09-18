module Secrets where

import Data.Maybe
import qualified System.Environment as Env

getDir :: IO FilePath
getDir = fromMaybe "../secrets" <$> Env.lookupEnv "SECRETS_DIR"
