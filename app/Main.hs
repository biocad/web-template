{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader (ask, lift, liftIO)
import           Data.Text            (unpack)
import           Web.Scotty.Trans     (get, text)
import           Web.Template         (CustomWebServer (..), Process (..),
                                       Route (..), runWebServer)



main :: IO ()
main = let myState = True
           myWebServer = CustomWebServer myState [ Route get 1 "/ping" pingR
                                                 , Route get 1 "/pong" pongR
                                                 ]
       in runWebServer myWebServer

pingR :: Process Bool
pingR = Process $ do
    state <- lift ask
    liftIO $ print state
    text "Pong!"

pongR :: Process Bool
pongR = AuthProcess $ \userId -> do
  liftIO . print $ "Authorised: " ++ unpack userId
  text "Ping!"
