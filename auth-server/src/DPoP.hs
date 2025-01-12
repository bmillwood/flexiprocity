-- | https://docs.bsky.app/docs/advanced-guides/oauth-client#dpop
module DPoP where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import qualified Data.Time as Time

import qualified Crypto.JOSE.Compact as JC
import qualified Crypto.JOSE.Error as JOSE
import qualified Crypto.JOSE.Header as Header
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.JOSE.JWS as JWS
import qualified Crypto.JWT as JWT
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.URI as URI

import qualified PKCE

createJwk :: IO JWK.JWK
createJwk = JWK.genJWK $ JWK.ECGenParam JWK.P_256

getNonce :: HTTP.Response a -> Maybe Text
getNonce resp = Text.decodeASCII <$> lookup "DPoP-Nonce" (HTTP.responseHeaders resp)

header :: JWK.JWK -> JWS.JWSHeader ()
header key =
  JWS.newJWSHeader ((), JWS.ES256)
  & Header.typ ?~ Header.HeaderParam () "dpop+jwt"
  & Header.jwk .~ (Header.HeaderParam () <$> key ^. JWK.asPublicKey)

data DPoPClaims = DPoPClaims
  { jwtClaims :: JWT.ClaimsSet
  , htm :: Text -- ^ HTTP method
  , htu :: Text -- ^ HTTP request URL
  , nonce :: Text -- ^ provided by the server
  }

instance Aeson.ToJSON DPoPClaims where
  toJSON DPoPClaims{ jwtClaims, htm, htu, nonce } =
    foldr ins (Aeson.toJSON jwtClaims)
      [ ("htm", Aeson.String htm)
      , ("htu", Aeson.String htu)
      , ("nonce", Aeson.String nonce)
      ]
    where
      ins (k, v) (Aeson.Object o) = Aeson.Object $ AesonKM.insert k v o
      ins _ j = j

instance JWT.HasClaimsSet DPoPClaims where
  claimsSet f s = fmap (\jwtClaims -> s { jwtClaims }) (f (jwtClaims s))

makeClaims :: HTTP.Method -> URI.URI -> Text -> IO DPoPClaims
makeClaims method uri nonce = do
  jti <- Text.decodeUtf8 <$> PKCE.makeVerifier
  now <- Time.getCurrentTime
  let
    roundedNow = now{ Time.utctDayTime = fromInteger $ round (Time.utctDayTime now) }
  pure $ DPoPClaims
    { jwtClaims =
        JWT.emptyClaimsSet
        & JWT.claimJti ?~ jti
        & JWT.claimIat ?~ JWT.NumericDate roundedNow
    , htm = Text.decodeASCII method
    , htu = Text.pack (URI.uriToString id uri "")
    , nonce
    }

dpopRequest :: Text -> JWK.JWK -> HTTP.Request -> IO HTTP.Request
dpopRequest nonce key req = do
  let
    method = HTTP.method req
    uri = HTTP.getUri req
  Right @JOSE.Error jwt <- JOSE.runJOSE
    $ JWT.signJWT key (header key) =<< liftIO (makeClaims method uri nonce)
  let jwtEnc = BSL.toStrict $ JC.encodeCompact jwt
  pure req
    { HTTP.requestHeaders = ("DPoP", jwtEnc) : HTTP.requestHeaders req }
