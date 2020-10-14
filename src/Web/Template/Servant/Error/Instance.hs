{-# OPTIONS_GHC -Wno-orphans #-}

module Web.Template.Servant.Error.Instance where

import Control.Monad.Except      (MonadError (..))
import Data.Aeson                (encode)
import Data.ByteString.Char8     (unpack)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status (..))
import Servant.Server            (ServerError (..))

import Web.Template.Except (MonadWebError (..))

instance {-# OVERLAPPING #-} MonadError ServerError m => MonadWebError m where
  throwJson (Status code reason) e = throwError $ ServerError
    { errHTTPCode = code
    , errReasonPhrase = unpack reason
    , errBody = encode e
    , errHeaders = [(hContentType, "application/json")]
    }

