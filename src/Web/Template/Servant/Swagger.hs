{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Template.Servant.Swagger
  ( WithDescription(..)
  , AsEnum(..)
  ) where

import Control.Lens                 (_Just, (%~), (&), (?~))
import Data.Aeson                   (FromJSON, ToJSON, Value (..))
import Data.OpenApi                 (ToSchema (..), defaultSchemaOptions, description, enum_,
                                     genericDeclareNamedSchema, schema)
import Data.OpenApi.Internal.Schema (GToSchema, rename)
import Data.Override                (Override)
import Data.Proxy                   (Proxy (..))
import Data.Text                    (Text, pack)
import Data.Typeable                (Typeable)
import GHC.Generics                 (Generic (..))
import GHC.TypeLits                 (KnownSymbol, Symbol, symbolVal)
import Type.Reflection              (typeRep)

-- | Add a description to any field in a record.
--
-- Intended to be used with 'Data.Override.Override' from @generic-override@ package like this:
--
-- @
-- data Foo =
--   { fooSomeField :: Text
--   , fooOtherField :: Int
--   }
--   deriving (Generic)
--   deriving (ToJSON, FromJSON, ToSchema) via
--     'Web.Template.Servant.Aeson.CamelCaseAeson' ('Data.Override.Override' Foo
--       '[ "fooSomeField" `'Data.Override.With'` 'WithDescription' "Some text describing a Foo"
--        , "fooOtherField" `'Data.Override.With'` 'WithDescription' "A number of Foos"
--        ])
-- @
--
-- For techincal reasons, @CamelCaseAeson@ must be on the outer layer.
newtype WithDescription (descr :: Symbol) a
  = WithDescription a
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

instance (Typeable a, KnownSymbol descr, ToSchema a) => ToSchema (WithDescription descr a) where
  declareNamedSchema _ = do
    sch <- declareNamedSchema @a Proxy
    return $ sch & schema . description ?~ pack (symbolVal @descr Proxy)

-- | Describe possible enumeration values for a 'Text' field.
--
-- To be used with 'Data.Override.Override':
--
-- @
-- data Foo =
--   { fooColor :: Text
--   }
--   deriving (Generic)
--   deriving (ToJSON, FromJSON, ToSchema) via
--     'Web.Template.Servant.Aeson.CamelCaseAeson' ('Data.Override.Override' Foo
--       '[ "fooColor" `'Data.Override.As`' 'AsEnum' '["red", "black", "white"]
--        ])
-- @
newtype AsEnum (vals :: [Symbol])
  = AsEnum Text
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema (AsEnum '[]) where
  declareNamedSchema _ = do
    sch <- declareNamedSchema @Text Proxy
    return $ sch & schema . enum_ ?~ []

instance (KnownSymbol val, Typeable vals, ToSchema (AsEnum vals)) => ToSchema (AsEnum (val ': vals)) where
  declareNamedSchema _ = do
    sch <- declareNamedSchema @(AsEnum vals) Proxy
    return $ sch & schema . enum_ . _Just %~ (String (pack $ symbolVal @val Proxy) :)

instance
  ( Typeable a
  , Typeable xs
  , Generic (Override a xs)
  , GToSchema (Rep (Override a xs))
  ) => ToSchema (Override a xs) where
    declareNamedSchema p =
      -- Prevent "Override" from showing up in schema name.
      rename (Just $ pack $ show $ typeRep @a) <$> genericDeclareNamedSchema defaultSchemaOptions p
