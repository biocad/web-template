module Web.Template.Wai
  where

import Control.Exception        (SomeException, fromException)
import Data.Text                (Text)
import Network.HTTP.Types       (Header)
import Network.Wai              (Middleware, Request, mapResponseHeaders, modifyResponse)
import Network.Wai.Handler.Warp (InvalidRequest (..), Port, Settings, defaultSettings,
                                 exceptionResponseForDebug, setOnException, setOnExceptionResponse,
                                 setPort)

import System.BCD.Log (error')

import Web.Template.Log (bcdlog, bcdlog400)

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

warpSettings :: Port -> (Settings -> Settings) -> Settings
warpSettings port userSettings =
  setOnException defaultOnException
  . setOnExceptionResponse exceptionResponseForDebug
  . setPort port
  . userSettings
  $ defaultSettings
