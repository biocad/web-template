{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.RWS     (asks, lift, tell)
import           Data.Text             (Text, pack)
import           Data.Text.Lazy        (fromStrict)
import           System.BCD.Log        (Level (..), MonadBCDLog (..),
                                        WithBCDLog)
import           System.Log.FastLogger (LoggerSet, defaultBufSize,
                                        newStderrLoggerSet)
import           Text.Printf           (printf)
import           Web.Scotty.Trans      (get, text)
import           Web.Template          (CustomWebServer (..), MonadWebError,
                                        Process (..), ProcessRW, Route (..),
                                        bcdLogFast, logException,
                                        restartOnError1, runWebServer,
                                        throwJson500)

type REnv = (Int, LoggerSet)

appName :: Text
appName = "web-template"

main :: IO ()
main = do
    ls <- newStderrLoggerSet defaultBufSize
    logger <- bcdLogFast ls appName
    let
      rEnv = (0, ls)
      myWebServer = CustomWebServer rEnv wEnv () [logger] [ Route get 1 "/ping" pingR
                                                          , Route get 2 "/ping" pingR2
                                                          , Route get 1 "/pong" pongR
                                                          , Route get 1 "/throw" throwR
                                                          , Route get 1 "/exception" exceptionR
                                                          ]
    restartOnError1 $ runWebServer 5000 myWebServer
  where
    wEnv = ["Start server"]

pingR :: ProcessRW REnv [Text]
pingR = Process $ do
    env <- lift $ asks fst
    lift $ tell ["Got /ping request"]
    -- This call will log "Main.logMsg:45" as location as there is no 'HasCallStack' in scope here.
    logMsg appName INFO "pingR"
    text . fromStrict . pack $ printf "Pong!\nCurrent environment: %d." env

pingR2 :: ProcessRW REnv [Text]
pingR2 = Process $ do
    env <- lift $ asks fst
    lift $ tell ["Got /ping request (version 2)"]
    text . fromStrict . pack $ printf "Pong of version 2!\nCurrent environment: %d." env

pongR :: ProcessRW REnv [Text]
pongR = AuthProcess $ \userId -> do
    lift $ tell ["Got /pong request"]
    text . fromStrict . pack $ printf "Ping!\nAuthorised: %s." userId

-- Demonstration of MTL-style.
-- Algorithm does not depend on ActionT, but can be called from it.
--
-- Observe that this computation is pure and therefore can be tested with mock instances.
algo :: (Monad m, WithBCDLog m, MonadWebError m) => m ()
algo = do
    -- This call will log "Main.algo:66" as source location because of 'WithBCDLog' constraint.
    logDebug appName "here"
    throwJson500 ("error" :: String, 42 :: Int)

throwR :: ProcessRW REnv [Text]
throwR = Process $ do
    algo

exceptionR :: ProcessRW REnv [Text]
exceptionR = Process $ logException appName $ do
    error "there is an error"
