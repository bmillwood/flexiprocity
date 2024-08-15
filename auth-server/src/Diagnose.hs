module Diagnose where

import Control.Exception
import Control.Monad
import qualified Data.Time as Time

addTimestamp :: String -> IO String
addTimestamp s = do
  now <- Time.getCurrentTime
  pure $ "[" <> show now <> "] " <> s

logMsg :: String -> IO ()
logMsg = putStrLn <=< addTimestamp

annotateException :: String -> IO a -> IO a
annotateException prefix act =
  act `catch` \e@SomeException{} -> logMsg (prefix <> ": " <> show e) >> throwIO e
