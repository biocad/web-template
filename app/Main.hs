{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.RWS (ask, lift, tell)
import           Data.Text         (Text, pack)
import           Data.Text.Lazy    (fromStrict)
import           Text.Printf       (printf)
import           Web.Scotty.Trans  (get, text)
import           Web.Template      (CustomWebServer (..),
                                    Process (..), ProcessRW, Route (..),
                                    defaultHandleLog, runWebServer)

main :: IO ()
main = runWebServer 5000 myWebServer
  where
    rEnv = 0
    wEnv = ["Start server"]
    myWebServer = CustomWebServer rEnv wEnv () defaultHandleLog [ Route get 1 "/ping" pingR
                                                                , Route get 2 "/ping" pingR2
                                                                , Route get 1 "/pong" pongR
                                                                ]

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
