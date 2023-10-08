module Twitter where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.List (sort)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Environment as Env
import qualified System.Random as Random
import qualified Data.Time.Clock.POSIX as Time

import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.ByteArray.Encoding as Enc
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import qualified Web.HttpApiData as Form
import qualified Web.FormUrlEncoded as Form
import qualified Network.HTTP.Types.URI as URI

getApiKey :: IO String
getApiKey = Env.getEnv "TWITTER_API_KEY"

getApiSecret :: IO String
getApiSecret = Env.getEnv "TWITTER_API_SECRET"

getAccessToken :: IO String
getAccessToken = Env.getEnv "TWITTER_ACCESS_TOKEN"

getAccessTokenSecret :: IO String
getAccessTokenSecret = Env.getEnv "TWITTER_ACCESS_TOKEN_SECRET"

genNonce :: IO String
genNonce = replicateM 32 randomAlphaNum
  where
    candidates = ['0' .. '9'] <> ['a' .. 'z'] <> ['A' .. 'Z']
    randomAlphaNum = do
      i <- Random.randomRIO (0, length candidates - 1)
      pure (candidates !! i)

data SignatureSecrets = SignatureSecrets
  { apiKey :: String
  , apiSecret :: String
  , accessToken :: Maybe String
  , accessTokenSecret :: Maybe String
  } deriving stock (Show)

data SignatureParams = SignatureParams
  { method :: ByteString
  , url :: Req.Url 'Req.Https
  , params :: [(Text, Text)]
  } deriving stock (Show)

data SignatureGen = SignatureGen
  { nonce :: Text
  , timestamp :: Text
  } deriving stock (Show)

signatureGen :: IO SignatureGen
signatureGen = do
  nonce <- Text.pack <$> genNonce
  timestamp <- Text.pack . show @Integer . floor <$> Time.getPOSIXTime
  pure SignatureGen{ nonce, timestamp }

computeSignature :: SignatureSecrets -> SignatureParams -> SignatureGen -> ByteString
computeSignature
    SignatureSecrets{ apiKey, apiSecret, accessToken, accessTokenSecret }
    SignatureParams{ method, url, params }
    SignatureGen{ nonce, timestamp } = do
  let
    oauth_token = maybe [] (pure . ("oauth_token" ,) . Text.pack) accessToken
    paramsToSign =
      [ ("oauth_consumer_key", Text.pack apiKey)
      , ("oauth_nonce", nonce)
      , ("oauth_signature_method", "HMAC-SHA1")
      , ("oauth_timestamp", timestamp)
      , ("oauth_version", "1.0")
      ] <> oauth_token <> params
    paramsString =
      BSL.toStrict
      . Form.urlEncodeParams
      . sort
      $ paramsToSign
    baseString =
      mconcat
        [ method
        , "&"
        , URI.urlEncode True . Text.encodeUtf8 $ Form.toUrlPiece (Req.renderUrl url)
        , "&"
        , URI.urlEncode True paramsString
        ]
    signingKey =
      BSL.toStrict
      $ BSB.toLazyByteString
      $ mconcat
        [ Form.toEncodedUrlPiece apiSecret
        , "&"
        , maybe "" Form.toEncodedUrlPiece accessTokenSecret
        ]
  Enc.convertToBase Enc.Base64
    (HMAC.hmac signingKey baseString :: HMAC.HMAC Hash.SHA1)

methodName :: forall a. (Req.HttpMethod a) => a -> ByteString
methodName _ = Req.httpMethodName (Proxy @a)

computeAuthorization
  :: SignatureSecrets -> SignatureParams -> SignatureGen -> ByteString
computeAuthorization
    secrets@SignatureSecrets{ apiKey, accessToken }
    sigParams
    gen@SignatureGen{ nonce, timestamp } =
  "OAuth " <> formatKeyValues keyValues
  where
    formatKeyValue (k, v) =
      URI.urlEncode True k
      <> "=\""
      <> URI.urlEncode True v
      <> "\""
    formatKeyValues = BSC.intercalate ", " . map formatKeyValue
    sig = computeSignature secrets sigParams gen
    keyValues =
      concat
        [ [ ("oauth_consumer_key", BSC.pack apiKey)
          , ("oauth_nonce", Text.encodeUtf8 nonce)
          , ("oauth_signature", sig)
          , ("oauth_signature_method", "HMAC-SHA1")
          , ("oauth_timestamp", Text.encodeUtf8 timestamp)
          ]
        , maybe [] (pure . ("oauth_token" ,) . BSC.pack) accessToken
        , [ ("oauth_version", "1.0")
          ]
        ]

requestToken :: IO ()
requestToken = do
  apiKey <- getApiKey
  apiSecret <- getApiSecret
  gen <- signatureGen
  let
    secrets =
      SignatureSecrets
        { apiKey
        , apiSecret
        , accessToken = Nothing
        , accessTokenSecret = Nothing
        }
    method = Req.POST
    url = Req.https "api.twitter.com" /: "oauth" /: "request_token"
    params =
      SignatureParams
        { method = methodName method
        , url
        , params = []
        }
    authorization =
      "OAuth " <> computeAuthorization secrets params gen
  resp <- Req.runReq Req.defaultHttpConfig
    $ Req.req
      Req.POST
      url
      Req.NoReqBody
      Req.lbsResponse
    $ Req.header "Authorization" authorization
  print resp
