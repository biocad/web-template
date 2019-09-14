{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Template.Log
  ( bcdlog
  , bcdLogFast
  ) where

import           Control.Monad.Trans                  (lift)
import           Data.Aeson                           (pairs, (.=))
import           Data.Aeson.Encoding                  (encodingToLazyByteString)
import           Data.Default                         (Default (..))
import           Data.Has                             (Has)
import           Data.Text                            (Text, pack, unpack)
import           Data.Text.Encoding                   (decodeUtf8)
import           Data.Time                            (ZonedTime,
                                                       defaultTimeLocale,
                                                       formatTime, parseTimeM)
import           GHC.Stack                            (withFrozenCallStack)
import           Network.HTTP.Types.Status            (Status (..))
import           Network.Wai                          (Middleware, rawPathInfo,
                                                       requestMethod)
import           Network.Wai.Logger                   (ZonedDate)
import           Network.Wai.Middleware.RequestLogger (Destination (..),
                                                       OutputFormat (..),
                                                       OutputFormatterWithDetails,
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.BCD.Log                       (Level (..),
                                                       MonadBCDLog (..))
import           System.IO.Unsafe                     (unsafePerformIO)
import           System.Log.FastLogger                (LoggerSet, toLogStr)
import           Web.Scotty.Trans                     (ActionT)

import           Web.Template.Types                   (Env)

{-# NOINLINE bcdlog #-}
bcdlog :: Middleware
bcdlog = unsafePerformIO $ mkRequestLogger def {outputFormat = CustomOutputFormatWithDetails (formatter "scotty")}

-- | Make @wai@ request logger that formats lines per BCD log format
-- and sends them to @fast-logger@'s 'LoggerSet'.
--
bcdLogFast
  :: LoggerSet     -- ^ Target logger
  -> Text          -- ^ Application name
  -> IO Middleware
bcdLogFast ls appName = mkRequestLogger
  def
    { destination  = Logger ls
    , outputFormat = CustomOutputFormatWithDetails (formatter appName)
    }

formatter :: Text -> OutputFormatterWithDetails
formatter appName zonedDate request status _ duration _ _ = do
    let statusC = statusCode status
    let method  = decodeUtf8 $ requestMethod request
    let url     = decodeUtf8 $ rawPathInfo request

    let msg' = method <> " " <> url <> " " <> pack (show statusC)

    -- Construct extended log record effectively by rendering directly to JSON, without
    -- intermediate Value step.
    let res = pairs
          (  "datetime"  .= toIso zonedDate
          <> "timestamp" .= toMs zonedDate
          <> "level"     .= INFO
          <> "app"       .= appName
          <> "msg"       .= msg'
          <> "status"    .= statusC
          <> "url"       .= url
          -- duration :: NominalDiffTime contains seconds, multiply by 1000 to get milliseconds
          <> "duration"  .= (realToFrac (duration * 1000) :: Double)
          )

    toLogStr (encodingToLazyByteString res) <> "\n"
  where
    toIso :: ZonedDate -> Text
    toIso = pack . maybe "1970-01-01T00:00:00+0000" (formatTime defaultTimeLocale "%FT%T%z") . parseZonedDate

    toMs :: ZonedDate -> Int
    toMs = (* 1000) . read . maybe "0" (formatTime defaultTimeLocale "%s") . parseZonedDate

    parseZonedDate :: ZonedDate -> Maybe ZonedTime
    parseZonedDate = parseTimeM True defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z" . unpack . decodeUtf8

-- | If your 'Web.Template.Types.WebM's Reader environment has a 'LoggerSet', this instance
-- will let you use logging in your routes.
--
instance {-# OVERLAPPING #-} (Has LoggerSet r, Monoid w) => MonadBCDLog (ActionT e (Env r w s)) where
  {-# INLINE logMsg #-}
  logMsg a l m = withFrozenCallStack $ lift $ logMsg a l m

  -- These definitions are duplicated to avoid extra entries in stack.
  -- Without this logger would report the location of this instance instead of
  -- the location of concrete usage.

  {-# INLINE logDebug #-}
  logDebug a = withFrozenCallStack $ logMsg a DEBUG

  {-# INLINE logInfo #-}
  logInfo a = withFrozenCallStack $ logMsg a INFO

  {-# INLINE logWarning #-}
  logWarning a = withFrozenCallStack $ logMsg a WARNING

  {-# INLINE logError #-}
  logError a = withFrozenCallStack $ logMsg a ERROR

  {-# INLINE logCritical #-}
  logCritical a = withFrozenCallStack $ logMsg a CRITICAL
