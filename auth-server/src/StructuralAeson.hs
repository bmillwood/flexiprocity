module StructuralAeson where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Data.Proxy
import Data.String
import GHC.TypeLits

newtype Field (s :: Symbol) a = Field a

instance (KnownSymbol s, Aeson.FromJSON a) => Aeson.FromJSON (Field s a) where
  parseJSON = Aeson.withObject "Field" $ \o ->
    Field <$> o .: fromString (symbolVal (Proxy @s))
