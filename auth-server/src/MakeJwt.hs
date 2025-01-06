module MakeJwt where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Time.Clock.POSIX as TimePosix

import qualified Crypto.JOSE.Compact as JOSE
import qualified Crypto.JOSE.Error as JOSE
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.JOSE.JWS as JWS
import qualified Crypto.JWT as JWT
import qualified Data.X509 as X509
import qualified Data.X509.File as X509
import qualified Web.Cookie as Cookie

import qualified Secrets

newtype Env = Env { privateKey :: JWK.JWK }

init :: IO Env
init = do
  secretsDir <- Secrets.getDir
  [X509.PrivKeyRSA privateKey]
    <- X509.readKeyFile (secretsDir <> "/jwt/private-key.pem")
  pure $ Env (JWK.fromRSA privateKey)

makeJwt :: Env -> Map.Map Text Aeson.Value -> IO BS.ByteString
makeJwt Env{ privateKey } claimsMap = do
  now <- TimePosix.getPOSIXTime
  let
    static = Map.fromList
      [ ("nbf", Aeson.toJSON (round now :: Integer))
      , ("exp", Aeson.toJSON (round (now + TimePosix.posixDayLength) :: Integer))
      , ("aud", Aeson.String "postgraphile")
      ]
    header = JWS.newJWSHeader ((), JWS.RS256)
  Right @JOSE.Error jwt <- JOSE.runJOSE
    $ JWT.signJWT privateKey header (Map.union static claimsMap)
  pure . BSL.toStrict $ JOSE.encodeCompact jwt

cookie :: Env -> Map.Map Text Aeson.Value -> IO BS.ByteString
cookie env claimsMap = ofEncoded <$> makeJwt env claimsMap
  where
    ofEncoded encodedJwt =
      Cookie.renderSetCookieBS Cookie.defaultSetCookie
        { Cookie.setCookieName = "jwt"
        , Cookie.setCookieValue = encodedJwt
        , Cookie.setCookiePath = Just "/"
        , Cookie.setCookieMaxAge = Just 86400
        , Cookie.setCookieHttpOnly = True
        , Cookie.setCookieSecure = True
        , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
        }
