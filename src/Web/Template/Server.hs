{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Web.Template.Server
    ( restartOnError
    , restartOnError1
    , runWebServer
    , runWebServerWith
    , defaultHandleLog
    , defaultHandleLog400
    , defaultHeaderCORS
    , toApplication
    ) where

import           Control.Concurrent        (threadDelay)
import           Control.Exception         (AsyncException (..), SomeException (..), catch,
                                            fromException)
import           Control.Monad             (unless)
import           Control.Monad.RWS         (RWST, evalRWST)
import           Data.String               (fromString)
import           Data.Text.Encoding        (encodeUtf8)
import           Data.Text.Lazy            as TL (Text, toStrict)
import           Network.HTTP.Types.Header (Header)
import           Network.HTTP.Types.Status (status401)
import           Network.Wai               (Application, Middleware, Request, mapResponseHeaders,
                                            modifyResponse)
import           Network.Wai.Handler.Warp  (InvalidRequest (..), Settings, defaultSettings,
                                            exceptionResponseForDebug, setOnException,
                                            setOnExceptionResponse, setPort)
import           System.BCD.Log            (error')
import           Web.Cookie                (parseCookiesText)
import           Web.Scotty.Trans          (Options (..), ScottyT, defaultHandler, header, json,
                                            middleware, next, param, scottyAppT, scottyOptsT,
                                            status)
import           Web.Template.Except       (Except, JsonWebError (..), handleEx)
import           Web.Template.Log          (bcdlog, bcdlog400)
import           Web.Template.Types

-- | Restart `f` on `error` after `1s`.
restartOnError1 :: IO () -> IO ()
restartOnError1 = flip restartOnError $ (10 :: Int) ^ (6 :: Int)

-- | Restart `f` on `error` after `delayUs`.
restartOnError :: IO () -> Int -> IO ()
restartOnError f delayUs = f `catch` handle
  where
    handle :: SomeException -> IO ()
    handle e =
      case fromException e of
        -- Program should be killable by Ctrl-C.
        Just UserInterrupt -> return ()
        _ -> do
            putStrLn $ "unexpected exception\n" <> show e
            putStrLn $ "server will be restarted in " <> show delayUs <> "us"
            threadDelay delayUs
            restartOnError f delayUs

-- | For given port and server settings run the server with default timeout (30 seconds).
runWebServer :: (Monoid w, Show w) => Port -> CustomWebServer r w s -> IO ()
runWebServer port s = scottyOptsT (scottyOpts port id) (evalCustomWebServer s) (toScottyT s)

-- | For given user settings, port and server settings run the server.
-- Setting port and exception handler via @userSettings@ will have no effect.
-- Use @port@ to set up port instead.
--
runWebServerWith
  :: (Monoid w, Show w)
  => (Settings -> Settings)
  -> Port
  -> CustomWebServer r w s
  -> IO ()
runWebServerWith userSettings port s = scottyOptsT (scottyOpts port userSettings) (evalCustomWebServer s) (toScottyT s)

toApplication :: (Monoid w, Show w) => CustomWebServer r w s -> IO Application
toApplication s = scottyAppT (evalCustomWebServer s) (toScottyT s)

toScottyT :: Monoid w => CustomWebServer r w s -> ScottyT Except (Env r w s) ()
toScottyT CustomWebServer {..} = do
    mapM_ middleware middlewares
    defaultHandler handleEx
    mapM_ runRoute routes

evalCustomWebServer :: Monad m => CustomWebServer r w s -> RWST r w s m b1 -> m b1
evalCustomWebServer CustomWebServer {..} = (fst <$>) . (\rws -> evalRWST rws readerEnv stateEnv)

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

runRoute :: Monoid w => Route r w s -> ScottyM r w s ()
runRoute Route {..} = method (fromString $ "/:version" <> path) (checkVersion version . auth $ process)

-- | Create @Options@ with given port and timeout.
-- If no timeout is given, it will be set to Warp's default (30 seconds).
--
scottyOpts :: Port -> (Settings -> Settings) -> Options
scottyOpts port userSettings = Options 1 warpSettings
  where
    warpSettings =
      setOnException onException
      . setOnExceptionResponse exceptionResponseForDebug
      . setPort port
      . userSettings
      $ defaultSettings

onException :: Maybe Request -> SomeException -> IO ()
onException _ e =
  case fromException e of
    -- This exception happens too often when using Chrome, thus we better ignore it.
    -- See https://github.com/yesodweb/wai/issues/421
    Just ConnectionClosedByPeer -> return ()
    _ -> error' ("scotty" :: Text) $ show e

auth :: Monoid w => Process r w s -> WebM r w s ()
auth (Process p) = p
auth (AuthProcess p) = do
    cookiesM <- header "Cookie"
    let idMaybe = cookiesM >>= getIdFromCookies
    case idMaybe of
        Just id' -> p id'
        Nothing -> do
            status status401
            json . JsonWebError $ "Authorization failed"

checkVersion :: Monoid w => Int -> WebM r w s () -> WebM r w s ()
checkVersion version route = do
    versionPath <- param "version"
    unless ("v" <> show version == versionPath) next
    route

getIdFromCookies :: TL.Text -> Maybe UserId
getIdFromCookies cookies = lookup "id" $ parseCookiesText $ encodeUtf8 $ toStrict cookies
