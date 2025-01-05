module BlueskyApi
  ( Handle, getHandle, validHandle
  ) where

import Data.Char
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Servant.API as Servant

newtype Handle = Handle { getHandle :: Text }
  deriving stock (Eq, Ord, Show)

validHandle :: Text -> Either Text Handle
validHandle t
  | Text.length t > 253 = Left "too long"
  | Text.any (not . isAscii) t = Left "contains non-ASCII characters"
  | otherwise = checkParts True t
  where
    checkParts firstCheck remaining = case Text.breakOn "." remaining of
      (before, after)
        | Text.null before -> Left "empty segment"
        | Text.length before > 63 -> Left "segment too long"
        | Text.any (not . segmentChar) before -> Left "contains invalid characters"
        | Text.take 1 before == "-" || Text.takeEnd 1 before == "-" ->
          Left "segments cannot start or end with a hyphen"
        | Text.null after ->
          if firstCheck
          then Left "must have >= 2 segments"
          else if Text.all isNumber (Text.take 1 before)
          then Left "last segment cannot start with a number"
          else Right (Handle (Text.toLower t))
        | otherwise -> checkParts False (Text.drop 1 after)
    segmentChar c = isAlphaNum c || c == '-'

instance Servant.FromHttpApiData Handle where
  parseUrlPiece = validHandle
