{-# LANGUAGE OverloadedStrings #-}

module Web.Template.Log
  ( bcdlog
  ) where

import qualified Data.ByteString.Char8                as BS8 (pack)
import           Data.Default                         (Default (..))
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text, pack, unpack)
import           Data.Text.Encoding                   (decodeUtf8)
import           Data.Time                            (ZonedTime,
                                                       defaultTimeLocale,
                                                       formatTime, parseTimeM)
import           Network.HTTP.Types.Status            (Status (..))
import           Network.Wai                          (Middleware, rawPathInfo,
                                                       requestMethod)
import           Network.Wai.Logger                   (ZonedDate)
import           Network.Wai.Middleware.RequestLogger (OutputFormat (..),
                                                       OutputFormatter,
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.BCD.Log                       (Level (..), Log (..),
                                                       format)
import           System.IO.Unsafe                     (unsafePerformIO)
import           System.Log.FastLogger                (toLogStr)

{-# NOINLINE bcdlog #-}
bcdlog :: Middleware
bcdlog = unsafePerformIO $ mkRequestLogger def {outputFormat = CustomOutputFormat formatter}

formatter :: OutputFormatter
formatter zonedDate request status _ = do
    let msg' = requestMethod request <> " " <> rawPathInfo request <> " " <> (BS8.pack . show . statusCode $ status)
    let log' = Log (toIso zonedDate) (toUnixTime zonedDate) INFO "scotty" (decodeUtf8 msg')
    (toLogStr . format $ log') <> "\n"
  where
    toIso :: ZonedDate -> Text
    toIso = pack . maybe "1970-01-01T00:00:00+0000" (formatTime defaultTimeLocale "%FT%T%z") . parseZonedDate

    toUnixTime :: ZonedDate -> Int
    toUnixTime = read . maybe "0" (formatTime defaultTimeLocale "%s") . parseZonedDate

    parseZonedDate :: ZonedDate -> Maybe ZonedTime
    parseZonedDate = parseTimeM True defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z" . unpack . decodeUtf8
