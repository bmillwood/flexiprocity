module Diagnose where

import Control.Exception
import qualified Data.Time as Time
import qualified System.IO as IO

addTimestamp :: String -> IO String
addTimestamp s = do
  now <- Time.getCurrentTime
  pure $ "[" <> show now <> "] " <> s

logMsg :: String -> IO ()
logMsg s = do
  putStrLn =<< addTimestamp s
  IO.hFlush IO.stdout

annotateException :: String -> IO a -> IO a
annotateException prefix act =
  act `catch` \e@SomeException{} -> logMsg (prefix <> ": " <> show e) >> throwIO e
