-- | See Confidential Client Authentication in https://atproto.com/specs/oauth#authorization-requests
module ClientAssertion where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IORef as IORef
import qualified Data.List.NonEmpty as NE
import Data.List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Ord
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as PosixTime

import qualified Crypto.JOSE.Compact as JC
import qualified Crypto.JOSE.Error as JOSE
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.JOSE.JWS as JWS
import qualified Crypto.JWT as JWT
import qualified Network.URI as URI

data JWKData = JWKData
  { expiresAt :: Time.UTCTime
  , jwk :: JWK.JWK
  }

data Env = Env
  { authJwks :: IORef.IORef (NE.NonEmpty JWKData)
  , ourClientId :: URI.URI
  }

jwkLifetime :: Time.NominalDiffTime
jwkLifetime = 86400

newKey :: IO JWKData
newKey = do
  -- in principle we should associate clients with kIds so we can figure out
  -- which are still in use, but in practice we'll just stop using the key far
  -- enough in advance of its expiration that I don't see it being an issue
  kId <- Text.pack . show <$> PosixTime.getPOSIXTime
  jwk <-
    JWK.genJWK (JWK.ECGenParam JWK.P_256)
    <&> (JWK.jwkUse ?~ JWK.Sig)
      . (JWK.jwkKid ?~ kId)
  expiresAt <- Time.addUTCTime jwkLifetime <$> Time.getCurrentTime
  pure JWKData{ expiresAt, jwk }

startRotatingKeys :: IORef.IORef (NE.NonEmpty JWKData) -> IO ()
startRotatingKeys authKeys = do
  thread <- Async.async . forever $ do
    -- we don't publish expiration times, so we don't need to adhere to them
    -- precisely
    threadDelay (round $ 1e6 * jwkLifetime / 4)
    now <- Time.getCurrentTime
    jwks <- filter (isNotExpired now) . NE.toList <$> IORef.readIORef authKeys
    newJwks <- case partition (isFarFromExpiry now) jwks of
      ([], _) -> (:| jwks) <$> newKey
      (k : ks, rs) -> pure $ k :| ks <> rs
    IORef.writeIORef authKeys newJwks
  Async.link thread
  where
    isNotExpired now JWKData{ expiresAt } = expiresAt > now
    isFarFromExpiry now JWKData{ expiresAt } =
      Time.diffUTCTime expiresAt now > jwkLifetime / 2

init :: URI.URI -> IO Env
init ourClientId = do
  authJwks <- IORef.newIORef . NE.singleton =<< newKey
  startRotatingKeys authJwks
  pure Env{ authJwks, ourClientId }

getPublicKeys :: Env -> IO JWK.JWKSet
getPublicKeys Env{ authJwks } = do
  jwks <- NE.toList <$> IORef.readIORef authJwks
  pure . JWK.JWKSet $ mapMaybe ((^. JWK.asPublicKey) . jwk) jwks

getLongestLivedKey :: Env -> IO JWK.JWK
getLongestLivedKey Env{ authJwks } = do
  jwk . maximumBy (comparing expiresAt) <$> IORef.readIORef authJwks

makeAssertion :: Env -> URI.URI -> IO BS.ByteString
makeAssertion env@Env{ ourClientId } authorizationServer = do
  jwk <- getLongestLivedKey env
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
