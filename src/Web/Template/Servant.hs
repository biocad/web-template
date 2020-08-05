module Web.Template.Servant
  ( runServantServer
  , runServantServerWith

  , module Web.Template.Servant.Aeson
  , module Web.Template.Servant.API
  , module Web.Template.Servant.Auth
  , module Web.Template.Servant.Error
  ) where

import Data.Proxy               (Proxy (..))
import Network.Wai.Handler.Warp (Settings, runSettings)
import Servant.Server           (ErrorFormatters, HasServer, Server, serveWithContext)

import Web.Template.Types (Port)
import Web.Template.Wai   (defaultHandleLog, defaultHeaderCORS, warpSettings)

import Web.Template.Servant.Aeson
import Web.Template.Servant.API
import Web.Template.Servant.Auth
import Web.Template.Servant.Error

runServantServer
  :: forall api
  .  (HasServer api '[ErrorFormatters])
  => Port
  -> Server api
  -> IO ()
runServantServer = runServantServerWith @api id

runServantServerWith
  :: forall api
  .  (HasServer api '[ErrorFormatters])
  => (Settings -> Settings)
  -> Port
  -> Server api
  -> IO ()
runServantServerWith userSettings port server =
  runSettings (warpSettings port userSettings)
    $ defaultHandleLog
    $ defaultHeaderCORS
    $ serveWithContext @api Proxy cbdContext
    $ server
