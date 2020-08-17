{-# LANGUAGE OverloadedStrings #-}

module Web.Template.Log
  ( bcdlog
  , bcdlog400
  ) where

import           Data.Aeson                           (pairs, (.=))
import           Data.Aeson.Encoding                  (encodingToLazyByteString)
import           Data.ByteString.Builder              (toLazyByteString)
import           Data.Default                         (Default (..))
import           Data.Text                            as T (Text, pack, unpack, unwords)
import           Data.Text.Encoding                   (decodeUtf8)
import qualified Data.Text.Lazy.Encoding              as TLE (decodeUtf8)
import           Data.Time                            (ZonedTime, defaultTimeLocale, formatTime,
                                                       nominalDiffTimeToSeconds, parseTimeM,
                                                       zonedTimeToUTC)
import           Data.Time.Clock.POSIX                (utcTimeToPOSIXSeconds)
import           Network.HTTP.Types.Status            (Status (..))
import           Network.Wai                          (Middleware, rawPathInfo, requestMethod)
import           Network.Wai.Logger                   (ZonedDate)
import           Network.Wai.Middleware.RequestLogger (OutputFormat (..),
                                                       OutputFormatterWithDetails, mkRequestLogger,
                                                       outputFormat)
import           System.BCD.Log                       (Level (..))
import           System.IO.Unsafe                     (unsafePerformIO)
import           System.Log.FastLogger                (toLogStr)

{-# NOINLINE bcdlog #-}
bcdlog :: Middleware
bcdlog = unsafePerformIO $ mkRequestLogger def
  { outputFormat = CustomOutputFormatWithDetails $ formatter False
  }

{-# NOINLINE bcdlog400 #-}
bcdlog400 :: Middleware
bcdlog400 = unsafePerformIO $ mkRequestLogger def
  { outputFormat = CustomOutputFormatWithDetails $ formatter True
  }

formatter :: Bool -> OutputFormatterWithDetails
formatter log400 zonedDate request status _ _ _ respBody = do
    let
      zonedTime = parseZonedDate zonedDate
      statusC   = statusCode status
      method    = decodeUtf8 $ requestMethod request
      url       = decodeUtf8 $ rawPathInfo request
      msg'      = T.unwords [method, url, pack (show statusC)]

      -- Construct extended log record effectively by rendering directly to JSON, without
      -- intermediate Value step.
      res = pairs
        (  "datetime"  .= toIso zonedTime
        <> "timestamp" .= toMs zonedTime
        <> "level"     .= INFO
        <> "app"       .= ("scotty" :: Text)
        <> "msg"       .= msg'
        <> "status"    .= statusC
        <> "url"       .= url
        <> if log400 && statusC >= 400
              then "response" .= (TLE.decodeUtf8 $ toLazyByteString respBody)
              else mempty
        )

    toLogStr (encodingToLazyByteString res) <> "\n"
  where
    toIso :: Maybe ZonedTime -> Text
    toIso = pack . maybe "1970-01-01T00:00:00+0000" (formatTime defaultTimeLocale "%FT%T%z")

    toMs :: Maybe ZonedTime -> Int
    toMs = maybe 0 (floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . zonedTimeToUTC)

    parseZonedDate :: ZonedDate -> Maybe ZonedTime
    parseZonedDate = parseTimeM True defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z" . unpack . decodeUtf8
