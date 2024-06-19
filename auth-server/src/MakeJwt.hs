module MakeJwt where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Time.Clock.POSIX as TimePosix

import qualified Web.JWT as JWT

makeJwt :: Map.Map Text Aeson.Value -> IO Text
makeJwt claimsMap = do
  now <- TimePosix.getPOSIXTime
  Just privateKey <- JWT.readRsaSecret <$> BS.readFile "../secrets/jwt/private-key.pem"
  let
    encodeSigner = JWT.EncodeRSAPrivateKey privateKey
    claims =
      mempty
        { JWT.nbf = JWT.numericDate now
        , JWT.exp = JWT.numericDate (now + TimePosix.posixDayLength)
        , JWT.aud = Left <$> JWT.stringOrURI "postgraphile"
        , JWT.unregisteredClaims =
            JWT.ClaimsMap claimsMap
        }
  pure $ JWT.encodeSigned encodeSigner mempty claims
