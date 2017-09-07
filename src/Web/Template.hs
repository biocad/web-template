module Web.Template
  ( Route (..), WebM, CustomWebServer(..)
  , UserId, Process (..)
  , ServerConfig (..)
  , runWebServer, runWebServerConf
  ) where

import           Web.Template.Server (CustomWebServer (..), Process (..),
                                      Route (..), runWebServer,
                                      runWebServerConf)
import           Web.Template.Types  (UserId, WebM, ServerConfig (..))
