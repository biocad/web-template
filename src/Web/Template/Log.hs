{-# LANGUAGE OverloadedStrings #-}

module Web.Template.Log
  (
    bcdlog
  ) where

import qualified Data.ByteString.Char8                as BS8 (pack)
import           Data.Default                         (Default (..))
import           Data.Monoid                          ((<>))
import           Data.Text.Encoding                   (decodeUtf8)
import           Network.HTTP.Types.Status            (Status (..))
import           Network.Wai                          (Middleware, rawPathInfo,
                                                       requestMethod)
import           Network.Wai.Middleware.RequestLogger (OutputFormat (..),
                                                       OutputFormatter,mkRequestLogger,
                                                       outputFormat)
import           System.BCD.Log                       (Level (..), Log (..),
                                                       format)
import           System.Log.FastLogger                (toLogStr)

bcdlog :: IO Middleware
bcdlog = mkRequestLogger $ def {outputFormat = CustomOutputFormat customFormatter}

customFormatter :: OutputFormatter
customFormatter _ request status _ = do
    let msg' = requestMethod request <> " " <>
               rawPathInfo request <> " " <>
               (BS8.pack . show . statusCode $ status)
    -- TODO: time and formatted time are not correct now. It can be parsed
    -- from first argument of 'OutputFormatter' and reformatted.
    let log' = Log "1970-01-01T00:00:00+0000" 0 INFO "scotty" (decodeUtf8 msg')
    toLogStr . format $ log'
