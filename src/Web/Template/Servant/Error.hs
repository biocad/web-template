{-# OPTIONS_GHC -Wno-orphans #-}

module Web.Template.Servant.Error
  ( MonadWebError(..)

  , cbdContext
  ) where

import Control.Monad.Except      (throwError)
import Data.Aeson                (encode, pairs, (.=))
import Data.Aeson.Encoding       (encodingToLazyByteString)
import Data.ByteString.Char8     (unpack)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status (..))
import Servant                   (Context (..), err400)
import Servant.Server            (ErrorFormatter, ErrorFormatters (..), Handler, ServerError (..),
                                  defaultErrorFormatters)

import Web.Template.Except (MonadWebError (..))

instance MonadWebError Handler where
  throwJson (Status code reason) e = throwError $ ServerError
    { errHTTPCode = code
    , errReasonPhrase = unpack reason
    , errBody = encode e
    , errHeaders = [(hContentType, "application/json")]
    }

cbdContext :: Context '[ErrorFormatters]
cbdContext = cbdErrorFormatters :. EmptyContext

cbdErrorFormatters :: ErrorFormatters
cbdErrorFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = fmtError
  , urlParseErrorFormatter = fmtError
  , headerParseErrorFormatter = fmtError
  }

fmtError :: ErrorFormatter
fmtError _ _ msg = err400
  { errBody = encodingToLazyByteString $ pairs $ "error" .= msg
  , errHeaders = [(hContentType, "application/json")]
  }
