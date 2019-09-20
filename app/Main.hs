{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.RWS (ask, lift, tell)
import           Data.Text         (Text, pack)
import           Data.Text.Lazy    (fromStrict)
import           Text.Printf       (printf)
import           Web.Scotty.Trans  (get, text)
import           Web.Template      (CustomWebServer (..), MonadWebError,
                                    Process (..), ProcessRW, Route (..),
                                    defaultHandleLog, defaultHeaderCORS,
                                    restartOnError1, runWebServer, throwJson500)

main :: IO ()
main = restartOnError1 $ runWebServer 5000 myWebServer
  where
    rEnv          = 0
    wEnv          = ["Start server"]
    myMiddlewares = [ defaultHandleLog  -- add this to use default logger
                    , defaultHeaderCORS -- add header CORS to response
                    ]
    myRoutes      = [ Route get 1 "/ping" pingR
                    , Route get 2 "/ping" pingR2
                    , Route get 1 "/pong" pongR
                    , Route get 1 "/throw" throwR
                    ]
    myWebServer = CustomWebServer rEnv wEnv () myMiddlewares myRoutes

pingR :: ProcessRW Int [Text]
pingR = Process $ do
    env <- lift ask
    lift $ tell ["Got /ping request"]
    text . fromStrict . pack $ printf "Pong!\nCurrent environment: %d." env

pingR2 :: ProcessRW Int [Text]
pingR2 = Process $ do
    env <- lift ask
    lift $ tell ["Got /ping request (version 2)"]
    text . fromStrict . pack $ printf "Pong of version 2!\nCurrent environment: %d." env

pongR :: ProcessRW Int [Text]
pongR = AuthProcess $ \userId -> do
    lift $ tell ["Got /pong request"]
    text . fromStrict . pack $ printf "Ping!\nAuthorised: %s." userId

-- Demonstration of MTL-style.
-- Algorithm does not depend on ActionT, but can be called from it.
algo :: MonadWebError m => m ()
algo = throwJson500 ("error" :: String, 42 :: Int)

throwR :: ProcessRW Int [Text]
throwR = Process $ do
    algo
