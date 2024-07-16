module MakeJwt where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Time.Clock.POSIX as TimePosix

import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Web.JWT as JWT

newtype Env = Env { privateKey :: RSA.PrivateKey }

init :: IO Env
init = do
  Just privateKey <- JWT.readRsaSecret <$> BS.readFile "../secrets/jwt/private-key.pem"
  pure $ Env privateKey

makeJwt :: Env -> Map.Map Text Aeson.Value -> IO Text
makeJwt Env{ privateKey } claimsMap = do
  now <- TimePosix.getPOSIXTime
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
