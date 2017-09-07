{-# LANGUAGE DeriveGeneric #-}

module Web.Template.Types
  ( UserId, Env
  , WebM, ScottyM
  , ServerConfig (..)
  ) where

import           Control.Monad.Reader (ReaderT)
import           Data.Text            (Text)
import           Options.Generic      (Generic, ParseRecord)
import           Web.Scotty.Trans     (ActionT, ScottyT)
import           Web.Template.Except  (Except)



type UserId = Text

type Env state = ReaderT state IO

type WebM state a = ActionT Except (Env state) a
type ScottyM state a = ScottyT Except (Env state) a

newtype ServerConfig = ServerConfig { port :: Int }
  deriving (Generic, Show)

instance ParseRecord ServerConfig
