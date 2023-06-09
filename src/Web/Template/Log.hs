{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Template.Log
  ( bcdlog
  , bcdlog400
  , debugLog
  , debugLogHandler

  , logMiddlewareCustom
  , AccessLogRecord(..)
  , formatTimeIso

  , userIdVaultKey
  , tokenVaultKey
  , pTokenVaultKey
  ) where

import           Control.Monad             (forM_, when)
import           Crypto.JWT                (ClaimsSet)
import           Data.Aeson                (fromEncoding, pairs, (.=))
import           Data.ByteString.Builder   (hPutBuilder, toLazyByteString)
import           Data.IORef                (IORef, newIORef, readIORef)
import           Data.Text                 as T (Text, pack, unwords)
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Text.Encoding.Error  as TE
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE (decodeUtf8With)
import qualified Data.Text.Lazy.IO         as TLIO
import           Data.Time                 (ZonedTime, defaultTimeLocale, formatTime,
                                            nominalDiffTimeToSeconds, utcToLocalZonedTime)
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import           Data.Vault.Lazy           (Key, insert, newKey)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status (Status (..))
import           Network.Wai               (Middleware, rawPathInfo, requestMethod, responseStatus,
                                            vault)
import           Network.Wai.Internal      (Response (..))
import           System.BCD.Log            (Level (..))
import           System.IO                 (hFlush, stdout)
import           System.IO.Unsafe          (unsafePerformIO)

userIdVaultKey :: Key (IORef (Maybe Text))
userIdVaultKey = unsafePerformIO newKey
{-# NOINLINE userIdVaultKey #-}

tokenVaultKey :: Key (IORef (Maybe Text))
tokenVaultKey = unsafePerformIO newKey
{-# NOINLINE tokenVaultKey #-}

pTokenVaultKey :: Key (IORef (Maybe ClaimsSet))
pTokenVaultKey = unsafePerformIO newKey
{-# NOINLINE pTokenVaultKey #-}

data AccessLogRecord
  = AccessLogRecord
      { alStart         :: !POSIXTime
      , alFinishApp     :: !POSIXTime
      , alFinishNetwork :: !POSIXTime
      , alMsg           :: !Text
      , alStatus        :: !Int
      , alURL           :: !Text
      , alUserId        :: !(Maybe Text)
      , alResponseBody  :: Maybe TL.Text
      }
  deriving (Show, Generic)

bcdlog :: Middleware
bcdlog = logMiddleware False

bcdlog400 :: Middleware
bcdlog400 = logMiddleware True

debugLog :: Middleware
debugLog = logMiddlewareCustom True $ Just debugLogHandler

logMiddleware :: Bool -> Middleware
logMiddleware log400 = logMiddlewareCustom log400 Nothing

logMiddlewareCustom :: Bool -> Maybe (AccessLogRecord -> IO ()) -> Middleware
logMiddlewareCustom log400 mLogAction app request respond = do
  let
    url = decodeUtf8 $ rawPathInfo request
    method = decodeUtf8 $ requestMethod request
  start <- getPOSIXTime
  startZoned <- utcToLocalZonedTime $ posixSecondsToUTCTime start

  -- Insert an empty IORef to the vault associated with request.
  -- This IORef will later be filled by authorization handler in the application.
  userIdRef <- newIORef Nothing
  tokenRef  <- newIORef Nothing
  ptokenRef <- newIORef Nothing

  let vaultWithEverything =
        insert tokenVaultKey tokenRef $
        insert pTokenVaultKey ptokenRef $
        insert userIdVaultKey userIdRef $
        vault request

  app request { vault = vaultWithEverything } $ \response -> do
    finishApp <- getPOSIXTime
    !rcv <- respond response
    finishNetwork <- getPOSIXTime

    -- Read userId written by the application.
    userId <- readIORef userIdRef

    let
      statusC = statusCode $ responseStatus response
      msg'    = T.unwords [method, url, pack (show statusC)]
      responseBody =
        case response of
          -- Logger from wai-extra also reads streaming responses,
          -- but those may be big.
          ResponseBuilder _ _ b -> Just b
          _                     -> Nothing
      responseBodyText = TLE.decodeUtf8With TE.lenientDecode . toLazyByteString <$> responseBody

    case mLogAction of
      Nothing -> do
        let
          logLine = pairs
            (  "datetime"      .= formatTimeIso startZoned
            <> "timestamp"     .= floor @_ @Int (toMs start)
            <> "duration"      .= toMs (finishApp - start)
            <> "send_duration" .= toMs (finishNetwork - finishApp)
            <> "level"         .= INFO
            <> "app"           .= ("scotty" :: Text)
            <> "msg"           .= msg'
            <> "status"        .= statusC
            <> "url"           .= url
            <> maybe mempty ("userId" .=) userId
            <> if log400 && statusC >= 400
                  then maybe mempty ("response" .=) responseBodyText
                  else mempty
            )

        hPutBuilder stdout (fromEncoding logLine <> "\n")
        hFlush stdout
      Just logAction -> do
        logAction AccessLogRecord
          { alStart = start
          , alFinishApp = finishApp
          , alFinishNetwork = finishNetwork
          , alMsg = msg'
          , alStatus = statusC
          , alURL = url
          , alUserId = userId
          , alResponseBody = responseBodyText
          }

    return rcv

formatTimeIso :: ZonedTime -> Text
formatTimeIso = pack . formatTime defaultTimeLocale "%FT%T%z"

toMs :: POSIXTime -> Double
toMs = realToFrac . (1000 *) . nominalDiffTimeToSeconds

debugLogHandler :: AccessLogRecord -> IO ()
debugLogHandler AccessLogRecord{..} = do
  let
    duration = toMs (alFinishApp - alStart)
    sendDuration = toMs (alFinishNetwork - alFinishApp)

  startZoned <- utcToLocalZonedTime $ posixSecondsToUTCTime alStart
  TIO.putStrLn $
    formatTimeIso startZoned
    <> " INFO "
    <> T.pack (show alStatus)
    <> " " <> alURL
    <> " " <> pack (show duration)
    <> " " <> pack (show sendDuration)
  when (alStatus >= 400) $
    forM_ alResponseBody TLIO.putStrLn

  hFlush stdout
