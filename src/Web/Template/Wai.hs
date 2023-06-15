module Web.Template.Wai
  ( logMiddlewareCustom
  , debugLogHandler
  , debugLog
  , formatTimeIso

  , module Web.Template.Wai
  ) where

import           Control.Exception        (SomeException, displayException, fromException)
import           Data.Aeson               (fromEncoding, object, pairs, (.=))
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Text.IO             as TIO
import           Data.Time                (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import           Data.Time.Clock.POSIX    (getPOSIXTime, posixSecondsToUTCTime)
import           Network.HTTP.Types       (Header, status500)
import           Network.Wai              (Middleware, Request (..), Response, mapResponseHeaders,
                                           modifyResponse, responseBuilder)
import           Network.Wai.Handler.Warp (InvalidRequest (..), Port, Settings, defaultSettings,
                                           setOnException, setOnExceptionResponse, setPort)
import           System.IO                (hFlush, stdout)

import System.BCD.Log (error')

import Web.Template.Log (bcdlog, bcdlog400, debugLog, debugLogHandler, logMiddlewareCustom, formatTimeIso)

defaultHandleLog :: Middleware
defaultHandleLog = bcdlog

-- | Log everything as 'defaultHandleLog' and also log response bodies for
-- 4xx and 5xx responses.
defaultHandleLog400 :: Middleware
defaultHandleLog400 = bcdlog400

defaultHeaderCORS :: Middleware
defaultHeaderCORS = modifyResponse (mapResponseHeaders addHeaderCORS)
  where
    addHeaderCORS :: [Header] -> [Header]
    addHeaderCORS headers = case lookup "Access-Control-Allow-Origin" headers of
        Just _  -> headers
        Nothing -> ("Access-Control-Allow-Origin", "*") : headers

defaultOnException :: Maybe Request -> SomeException -> IO ()
defaultOnException _ e =
  case fromException e of
    -- This exception happens too often when using Chrome, thus we better ignore it.
    -- See https://github.com/yesodweb/wai/issues/421
    Just ConnectionClosedByPeer -> return ()
    _                           -> error' ("scotty" :: Text) $ show e

debugOnException :: Maybe Request -> SomeException -> IO ()
debugOnException req e =
  case fromException e of
    Just ConnectionClosedByPeer -> return ()
    _ -> do
      time <- getPOSIXTime >>= utcToLocalZonedTime . posixSecondsToUTCTime
      let
        msg = pack (formatTime defaultTimeLocale "%FT%T%z" time) <> " ERROR"
        exc = pack $ displayException e
        reqMsg =
          case req of
            Nothing -> ""
            Just request -> let
              url = decodeUtf8 $ rawPathInfo request
              method = decodeUtf8 $ requestMethod request
              in " " <> method <> " " <> url

      TIO.putStrLn $ msg <> reqMsg <> "\n" <> exc
      hFlush stdout

defaultExceptionResponse :: SomeException -> Response
defaultExceptionResponse e = responseBuilder status500 [] $ fromEncoding $ pairs
  (  "error" .= ("exception" :: Text)
  <> ("params" .= object
    [ "message" .= displayException e
    ])
  )

warpSettings :: Port -> (Settings -> Settings) -> Settings
warpSettings port userSettings =
  userSettings
  . setOnException defaultOnException
  . setOnExceptionResponse defaultExceptionResponse
  . setPort port
  $ defaultSettings
