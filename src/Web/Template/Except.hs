{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Web.Template.Except
  (
    Except (..)
  , JsonWebError (..)
  , ScottyError (..)
  , handleEx
  , MonadWebError(..)
  ) where

import Data.Aeson                (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding)
import Data.String               (fromString)
import GHC.Generics              (Generic)
import Network.HTTP.Types.Status (Status, status403, status404, status500)
import Web.Scotty.Trans          (ActionT, ScottyError (..), json, raise, status)


instance ScottyError Except where
    stringError = JsonException
    showError = fromString . show

data Except
  = Forbidden
  | NotFound !Int
  | StringException !String
  | JsonException !String
  | forall a. (Show a, ToJSON a) => CustomJsonException { cStatus :: !Status, cError :: !a }

deriving instance Show Except

newtype JsonWebError
  = JsonWebError { error :: String }
  deriving (Generic)

instance ToJSON JsonWebError where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON JsonWebError

handleEx :: Monad m => Except -> ActionT Except m ()
handleEx Forbidden = do
    status status403
    json . JsonWebError $ "Forbidden from server"
handleEx (NotFound i) = do
    status status404
    json . JsonWebError $ "Can't find " ++ show i
handleEx (StringException str)  = do
    status status500
    json . JsonWebError $ "Server problem: " ++ str
handleEx (JsonException str)  = do
    status status500
    json . JsonWebError $ str
handleEx CustomJsonException{..} = do
    status cStatus
    json cError

-- | MTL-style type class for monads that can throw user-visible HTTP errors.
--
class MonadWebError m where
  -- | Throw any 'ToJSON'able value with custom HTTP code. The value will be sent
  -- directly as response without any additional formatting.
  --
  throwJson :: (Show e, ToJSON e) => Status -> e -> m a

  -- | Specialized version of 'throwJson' that uses @500 Internal Server Error@ code.
  {-# INLINE throwJson500 #-}
  throwJson500 :: (Show e, ToJSON e) => e -> m a
  throwJson500 = throwJson status500

instance {-# OVERLAPPING #-} Monad m => MonadWebError (ActionT Except m) where
  {-# INLINE throwJson #-}
  throwJson s e = raise $ CustomJsonException s e
