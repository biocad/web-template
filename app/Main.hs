{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader (ask, lift)
import           Data.Text            (pack)
import           Data.Text.Lazy       (fromStrict)
import           Text.Printf          (printf)
import           Web.Scotty.Trans     (get, text)
import           Web.Template         (CustomWebServer (..), Process (..),
                                       Route (..), runWebServer)


main :: IO ()
main = runWebServer 5000 myWebServer
  where env = 0
        myWebServer = CustomWebServer env [ Route get 1 "/ping" pingR
                                          , Route get 1 "/pong" pongR
                                          ]

pingR :: Process Int
pingR = Process $ do
    env <- lift ask
    text . fromStrict . pack $ printf "Pong!\nCurrent environment: %d." env

pongR :: Process Int
pongR = AuthProcess $ \userId ->
    text . fromStrict . pack $ printf "Ping!\nAuthorised: %s." userId
