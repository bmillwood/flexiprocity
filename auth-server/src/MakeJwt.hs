module MakeJwt where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Time.Clock.POSIX as TimePosix

import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Data.X509 as X509
import qualified Data.X509.File as X509
import qualified Jose.Jwa as Jwa
import qualified Jose.Jws as Jws
import qualified Jose.Jwt as Jwt
import qualified Web.Cookie as Cookie

import qualified Secrets

newtype Env = Env { privateKey :: RSA.PrivateKey }

init :: IO Env
init = do
  secretsDir <- Secrets.getDir
  [X509.PrivKeyRSA privateKey]
    <- X509.readKeyFile (secretsDir <> "/jwt/private-key.pem")
  pure $ Env privateKey

makeJwt :: Env -> Map.Map Text Aeson.Value -> IO BS.ByteString
makeJwt Env{ privateKey } claimsMap = do
  now <- TimePosix.getPOSIXTime
  let
    static = Map.fromList
      [ ("nbf", Aeson.toJSON (round now :: Integer))
      , ("exp", Aeson.toJSON (round (now + TimePosix.posixDayLength) :: Integer))
      , ("aud", Aeson.String "postgraphile")
      ]
    claimsBS = BSL.toStrict $ Aeson.encode (Map.union static claimsMap)
  Right jwt <- Jws.rsaEncode Jwa.RS256 privateKey claimsBS
  pure (Jwt.unJwt jwt)

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
