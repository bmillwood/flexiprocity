-- | See Confidential Client Authentication in https://atproto.com/specs/oauth#authorization-requests
module ClientAssertion where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IORef as IORef
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as PosixTime

import qualified Crypto.JOSE.Compact as JC
import qualified Crypto.JOSE.Error as JOSE
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.JOSE.JWS as JWS
import qualified Crypto.JWT as JWT
import qualified Network.URI as URI

data Env = Env
  { authJwks :: IORef.IORef JWK.JWKSet
  , ourClientId :: URI.URI
  }

init :: URI.URI -> IO Env
init ourClientId = do
  -- eventually we'll want to store the kId we used for each client so we can
  -- avoid rotating keys that are in use
  kId <- Text.pack . show <$> PosixTime.getPOSIXTime
  authJwk <-
    JWK.genJWK (JWK.ECGenParam JWK.P_256)
    <&> (JWK.jwkUse ?~ JWK.Sig)
      . (JWK.jwkKid ?~ kId)
  authJwks <- IORef.newIORef (JWK.JWKSet [authJwk])
  pure Env{ authJwks, ourClientId }

getPublicKeys :: Env -> IO JWK.JWKSet
getPublicKeys Env{ authJwks } = do
  JWK.JWKSet jwks <- IORef.readIORef authJwks
  pure . JWK.JWKSet $ mapMaybe (^. JWK.asPublicKey) jwks

makeAssertion :: Env -> URI.URI -> IO BS.ByteString
makeAssertion Env{ authJwks, ourClientId } authorizationServer = do
  JWK.JWKSet (jwk : _) <- IORef.readIORef authJwks
  now <- Time.getCurrentTime
  Right @JOSE.Error jwt <- JOSE.runJOSE
    $ JWT.signJWT jwk (header jwk) (claims now)
  pure . BSL.toStrict $ JC.encodeCompact jwt
  where
    header jwk =
      JWS.newJWSHeader ((), JWS.ES256)
      & JWS.kid .~ (JWS.HeaderParam () <$> jwk ^. JWK.jwkKid)
    claims now =
      JWT.emptyClaimsSet
      & JWT.claimJti ?~ Text.pack (show now)
      & JWT.claimIss ?~ ourClientId ^. re JWT.uri
      & JWT.claimSub ?~ ourClientId ^. re JWT.uri
      & JWT.claimAud ?~ JWT.Audience [authorizationServer ^. re JWT.uri]
      & JWT.claimIat ?~ JWT.NumericDate (Time.addUTCTime (-1) now)
      & JWT.claimExp ?~ JWT.NumericDate (Time.addUTCTime 300 now)
