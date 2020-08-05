module Web.Template.Servant.Error
  ( cbdContext
  ) where

import Data.Aeson                (pairs, (.=))
import Data.Aeson.Encoding       (encodingToLazyByteString)
import Network.HTTP.Types.Header (hContentType)
import Servant                   (Context (..), err400)
import Servant.Server            (ErrorFormatter, ErrorFormatters (..), ServerError (..),
                                  defaultErrorFormatters)

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
