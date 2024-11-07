module Sentry where

import qualified Data.Aeson as Aeson
import qualified Control.Exception as Exception
import qualified GHC.Stack as Stack
import qualified System.Environment as Env
import qualified System.IO as IO

import qualified System.Log.Raven as Raven
import qualified System.Log.Raven.Types as Raven
import qualified System.Log.Raven.Transport.HttpConduit as Transport

newtype Service = Service (Maybe Raven.SentryService)

init :: IO Service
init =
  Env.lookupEnv "SENTRY_DSN" >>= \case
    Nothing -> pure (Service Nothing)
    Just dsn ->
      Service . Just <$> Raven.initRaven
        dsn
        id
        Transport.sendRecord
        Raven.stderrFallback

send :: Service -> String -> [(String, String)] -> [(String, Aeson.Value)] -> IO ()
send (Service Nothing) msg tags extra = IO.hPutStr IO.stderr $ unlines
  [ "[sentry disabled] " <> msg
  , "tags: " <> show tags <> ", extra: " <> show extra
  ]
send (Service (Just sentry)) msg tags extra =
  Raven.register
    sentry
    "auth-server"
    Raven.Error
    msg
    (Raven.tags tags . Raven.extra extra)

reportException :: Stack.HasCallStack => Service -> IO a -> IO a
reportException service act =
  Exception.catch act (\e@Exception.SomeException{} -> do
    send service ("Exception: " <> show e)
      [ ("exception", show e)
      , ("callStack", Stack.prettyCallStack Stack.callStack)
      ]
      []
    Exception.throw e
  )
