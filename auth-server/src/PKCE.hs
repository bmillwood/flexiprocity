module PKCE
  ( PKCE(..), makePKCE
  , fromVerifier, makeVerifier
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified System.Random as Random

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray.Encoding as Enc

-- | https://www.oauth.com/oauth2-servers/pkce/
data PKCE = PKCE
  { -- | Put this in code_verifier on your authorization code exchange
    verifier :: BS.ByteString
  , -- | Put this in code_challenge in your authorization request,
    -- alongside code_challenge_method=S256
    challenge :: BS.ByteString
  } deriving stock (Eq, Ord, Show)

-- | Useful in tests, to confirm the hash + encode is what you expect.
fromVerifier :: BS.ByteString -> PKCE
fromVerifier verifier = PKCE{ verifier, challenge }
  where
    challenge =
      Enc.convertToBase Enc.Base64URLUnpadded
        (Hash.hash verifier :: Hash.Digest Hash.SHA256)

-- | Generates a 128-char random string from a-zA-Z0-9-._~
makeVerifier :: IO BS.ByteString
makeVerifier =
  BSC.pack
  . map (characters !!)
  . take 128
  . Random.randomRs (0, length characters - 1)
  <$> Random.newStdGen
  where
    characters = concat
      [ ['a' .. 'z']
      , ['A' .. 'Z']
      , ['0' .. '9']
      , ['-', '.', '_', '~']
      ]

makePKCE :: IO PKCE
makePKCE = fromVerifier <$> makeVerifier
