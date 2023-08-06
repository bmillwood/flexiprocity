module MakeJwt where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Time.Clock.POSIX as TimePosix

import qualified Web.JWT as JWT

import qualified Facebook

makeJwt :: Facebook.UserId 'Facebook.FromFacebook -> IO Text
makeJwt fbUserId = do
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
            JWT.ClaimsMap (Map.singleton "facebookUserId" (Aeson.toJSON fbUserId))
        }
  pure $ JWT.encodeSigned encodeSigner mempty claims
