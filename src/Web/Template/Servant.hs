module Web.Template.Servant
  ( runServantServer
  , runServantServerWith
  , runServantServerWithContext

  , SwaggerSchemaUI
  , swaggerSchemaUIServer

  , module Web.Template.Servant.Aeson
  , module Web.Template.Servant.API
  , module Web.Template.Servant.Auth
  , module Web.Template.Servant.Error
  ) where

import Data.Proxy               (Proxy (..))
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Servant.Swagger.UI       (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Server           (Context, DefaultErrorFormatters, ErrorFormatters, HasContextEntry,
                                 HasServer, Server, serveWithContext, type (.++), (.++))

import Web.Template.Types (Port)
import Web.Template.Wai   (defaultHandleLog, defaultHeaderCORS, warpSettings)

import Web.Template.Servant.API
import Web.Template.Servant.Aeson
import Web.Template.Servant.Auth
import Web.Template.Servant.Error

runServantServer
  :: forall api
  .  (HasServer api '[ErrorFormatters])
  => Port
  -> Server api
  -> IO ()
runServantServer = runServantServerWith @api id (defaultHeaderCORS . defaultHandleLog)

runServantServerWith
  :: forall api
  .  (HasServer api '[ErrorFormatters])
  => (Settings -> Settings)
  -> (Application -> Application)
     -- ^ Middlewares
  -> Port
  -> Server api
  -> IO ()
runServantServerWith userSettings middlewares port server =
  runSettings (warpSettings port userSettings)
    $ middlewares
    $ serveWithContext @api Proxy cbdContext
    $ server

runServantServerWithContext
  :: forall api ctx
  .  (HasServer api (ctx .++ '[ErrorFormatters]), HasContextEntry ((ctx .++ '[ErrorFormatters]) .++ DefaultErrorFormatters) ErrorFormatters)
  => (Settings -> Settings)
  -> (Application -> Application)
     -- ^ Middlewares
  -> Port
  -> Context ctx
  -> Server api
  -> IO ()
runServantServerWithContext userSettings middlewares port ctx server =
  runSettings (warpSettings port userSettings)
    $ middlewares
    $ serveWithContext @api Proxy (ctx .++ cbdContext)
    $ server
