{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Template.Log
  ( bcdlog
  , bcdlog400
  ) where

import           Data.Aeson                (fromEncoding, pairs, (.=))
import           Data.ByteString.Builder   (hPutBuilder, toLazyByteString)
import           Data.Text                 as T (Text, pack, unwords)
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Text.Encoding.Error  as TE
import qualified Data.Text.Lazy.Encoding   as TLE (decodeUtf8With)
import           Data.Time                 (ZonedTime, defaultTimeLocale, formatTime,
                                            nominalDiffTimeToSeconds, utcToLocalZonedTime)
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import           Network.HTTP.Types.Status (Status (..))
import           Network.Wai               (Middleware, rawPathInfo, requestMethod, responseStatus)
import           Network.Wai.Internal      (Response (..))
import           System.BCD.Log            (Level (..))
import           System.IO                 (stdout)

bcdlog :: Middleware
bcdlog = logMiddleware False

bcdlog400 :: Middleware
bcdlog400 = logMiddleware True

logMiddleware :: Bool -> Middleware
logMiddleware log400 app request respond = do
  let
    url = decodeUtf8 $ rawPathInfo request
    method = decodeUtf8 $ requestMethod request
  start <- getPOSIXTime
  startZoned <- utcToLocalZonedTime $ posixSecondsToUTCTime start

  app request $ \response -> do
    finishApp <- getPOSIXTime
    !rcv <- respond response
    finishNetwork <- getPOSIXTime
    let
      statusC = statusCode $ responseStatus response
      msg'    = T.unwords [method, url, pack (show statusC)]
      responseBody =
        case response of
          -- Logger from wai-extra also reads streaming responses,
          -- but those may be big.
          ResponseBuilder _ _ b -> Just b
          _                     -> Nothing
      logLine = pairs
        (  "datetime"      .= toIso startZoned
        <> "timestamp"     .= floor @_ @Int (toMs start)
        <> "duration"      .= toMs (finishApp - start)
        <> "send_duration" .= toMs (finishNetwork - finishApp)
        <> "level"         .= INFO
        <> "app"           .= ("scotty" :: Text)
        <> "msg"           .= msg'
        <> "status"        .= statusC
        <> "url"           .= url
        <> if log400 && statusC >= 400
              then maybe mempty (\b -> "response" .= TLE.decodeUtf8With TE.lenientDecode (toLazyByteString b)) responseBody
              else mempty
        )

    hPutBuilder stdout (fromEncoding logLine <> "\n")

    return rcv
  where
    toIso :: ZonedTime -> Text
    toIso = pack . formatTime defaultTimeLocale "%FT%T%z"

    toMs :: POSIXTime -> Double
    toMs = realToFrac . (1000 *) . nominalDiffTimeToSeconds
