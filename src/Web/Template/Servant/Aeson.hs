module Web.Template.Servant.Aeson where

import Control.Lens      ((?~))
import Data.Aeson
import Data.Aeson.Casing
import Data.Functor      ((<&>))
import Data.Proxy        (Proxy (..))
import Data.Text         (pack)
import Data.Typeable     (Typeable)
import GHC.Generics
import Type.Reflection   (typeRep)

import Data.OpenApi
import Data.OpenApi.Internal.Schema
import Data.Override                (Override)

-- | This wrapper is intended to be used with @DerivingVia@ to make
-- consistent 'ToJSON', 'FromJSON' and 'ToSchema' for some data type.
--
-- Usage:
--
-- > data Foo
-- >   = Foo
-- >       { fFoo :: String
-- >       , fBar :: String
-- >       }
-- >   deriving (Eq, Show, Generic)
-- >   deriving (ToJSON, FromJSON, ToSchema) via CamelCaseAeson Foo
--
-- Instances are made with 'aesonPrefix' 'camelCase' and 'omitNothingFields' set to @True@.
newtype CamelCaseAeson a
  = CamelCaseAeson a

prefixOptions :: Options
prefixOptions = (aesonPrefix camelCase) { omitNothingFields = True }

instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (CamelCaseAeson a) where
  toJSON (CamelCaseAeson a) = genericToJSON prefixOptions a
  toEncoding (CamelCaseAeson a) = genericToEncoding prefixOptions a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CamelCaseAeson a) where
  parseJSON = fmap CamelCaseAeson . genericParseJSON prefixOptions

instance {-# OVERLAPS #-}
  ( Typeable a
  , Typeable xs
  , Generic (Override a xs)
  , GToSchema (Rep (Override a xs))
  ) => ToSchema (CamelCaseAeson (Override a xs)) where
  declareNamedSchema _ =
    -- Prevent "Override" from showing up in schema name.
    rename (Just $ pack $ show $ typeRep @a) <$>
      genericDeclareNamedSchema @(Override a xs) (fromAesonOptions prefixOptions) Proxy

instance {-# OVERLAPS #-} (Generic a, GToSchema (Rep a), Typeable a) => ToSchema (CamelCaseAeson a) where
    declareNamedSchema _ =
      genericDeclareNamedSchema @a (fromAesonOptions prefixOptions) Proxy

-- | This wrapper extends 'ToSchema' instance of the underlying type with
-- an example obtained from 'WithExample' instance.
--
-- Usage:
--
-- > data Foo
-- >   = ...
-- >   deriving (Eq, Show, Generic)
-- >   deriving (ToJSON, FromJSON) via CamelCaseAeson Foo
-- >   deriving (ToSchema) via SwaggerWithExample (CamelCaseAeson Foo)
--
-- Last line reuses 'ToSchema' instances from 'CamelCaseAeson' to ensure that instances
-- stay consistent.
newtype SwaggerWithExample a
  = SwaggerWithExample a

-- | Provide an example for Swagger schema.
--
-- Swagger supports only one example per named schema.
class ToJSON a => WithExample a where
  mkExample :: a

instance (WithExample a, ToSchema a) => ToSchema (SwaggerWithExample a) where
  declareNamedSchema _ = declareNamedSchema @a Proxy
    <&> schema . example ?~ toJSON (mkExample @a)

instance (ToJSON (CamelCaseAeson a), WithExample a) => WithExample (CamelCaseAeson a) where
  mkExample = CamelCaseAeson $ mkExample @a
