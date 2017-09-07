module Web.Template
  ( Route (..), WebM, CustomWebServer(..)
  , UserId, Process (..)
  , runWebServer
  ) where

import           Web.Template.Server (CustomWebServer (..), Process (..),
                                      Route (..), runWebServer)
import           Web.Template.Types  (UserId, WebM)
