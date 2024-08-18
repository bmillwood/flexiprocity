module StructuralAeson where

import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))

newtype Embedded a = Embedded a deriving (Show)

instance Aeson.FromJSON a => Aeson.FromJSON (Embedded a) where
  parseJSON = Aeson.withText "Embedded" $ \inner ->
    either fail (pure . Embedded)
      $ Aeson.eitherDecode $ BSL.fromStrict $ Text.encodeUtf8 inner

data Literal (s :: Symbol) = Literal

instance (KnownSymbol s) => Show (Literal s) where
  show lit = show (symbolVal lit)

instance (KnownSymbol s) => Aeson.FromJSON (Literal s) where
  parseJSON = Aeson.withText "Literal" $ \t ->
    if t == Text.pack expected
    then pure result
    else fail $ "Expected: " <> show expected <> ", actual: " <> show t
    where
      result :: Literal s = Literal
      expected = symbolVal result

newtype At (s :: Symbol) a = At a
  deriving stock (Generic, Show)

instance (KnownSymbol s, Aeson.FromJSON a) => Aeson.FromJSON (At s a) where
  parseJSON = Aeson.withObject ("At " <> show key) $ \o ->
    At <$> o .: fromString key
    where
      key = symbolVal (Proxy :: Proxy s)
