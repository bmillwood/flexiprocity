module Task where

import Data.Text (Text)

data Task
  = SendEmails
  | DisableEmails [Text]
