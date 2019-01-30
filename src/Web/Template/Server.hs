{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Template.Server
  ( restartOnError
  , restartOnError1
  , runWebServer
  , defaultHandleLog
  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Exception         (SomeException, catch)
import           Control.Monad             (unless)
import           Control.Monad.RWS         (evalRWST)
import           Data.String               (fromString)
import           Data.Text.Encoding        (encodeUtf8)
import           Data.Text.Lazy            as TL (Text, toStrict)
import           Network.HTTP.Types.Status (status401)
import           Network.Wai               (Middleware)
import           Network.Wai.Handler.Warp  (defaultSettings,
                                            exceptionResponseForDebug,
                                            setOnExceptionResponse, setPort)
import           Web.Cookie                (parseCookiesText)
import           Web.Scotty.Trans          (Options (..), defaultHandler,
                                            header, json, middleware, next,
                                            param, scottyOptsT, status)
import           Web.Template.Except       (JsonWebError (..), handleEx)
import           Web.Template.Log          (bcdlog)
import           Web.Template.Types

-- | Restart `f` on `error` after `1s`.
restartOnError1 :: IO () -> IO ()
restartOnError1 = flip restartOnError $ (10 :: Int) ^ (6 :: Int)

-- | Restart `f` on `error` after `delayUs`.
restartOnError :: IO () -> Int -> IO ()
restartOnError f delayUs = f `catch` handle
  where
    handle :: SomeException -> IO ()
    handle e = do putStrLn $ "unexpected exception\n" ++ show e
                  putStrLn $ "server will be restarted in " ++ show delayUs ++ "us"
                  threadDelay delayUs
                  restartOnError f delayUs

-- | For given port and server settings run the server.
runWebServer :: (Monoid w, Show w) => Port -> CustomWebServer r w s -> IO ()
runWebServer port CustomWebServer {..} =
  scottyOptsT (scottyOpts port) ((fst <$>) . (\rws -> evalRWST rws readerEnv stateEnv)) $ do
      mapM_ middleware middlewares
      defaultHandler handleEx
      mapM_ runRoute routes

defaultHandleLog :: Middleware
defaultHandleLog = bcdlog

runRoute :: Monoid w => Route r w s -> ScottyM r w s ()
runRoute Route{..} = method (fromString $ "/:version" ++ path) (checkVersion version . auth $ process)

scottyOpts :: Port -> Options
scottyOpts port = Options 1 warpSettings
  where warpSettings = setOnExceptionResponse exceptionResponseForDebug .
                       setPort port $ defaultSettings

auth :: Monoid w => Process r w s -> WebM r w s ()
auth (Process p) = p
auth (AuthProcess p) = do
    cookiesM <- header "Cookie"
    let idMaybe = cookiesM >>= getIdFromCookies
    case idMaybe of
        Just id' -> p id'
        Nothing -> do status status401
                      json . JsonWebError $ "Authorization failed"

checkVersion :: Monoid w => Int -> WebM r w s () -> WebM r w s ()
checkVersion version route = do
    versionPath <- param "version"
    unless ("v" ++ show version == versionPath) next
    route

getIdFromCookies :: TL.Text -> Maybe UserId
getIdFromCookies cookies = lookup "id" $ parseCookiesText $ encodeUtf8 $ toStrict cookies

