module Web.Template.Types
  (
    UserId
  , Port
  , Env
  , WebM
  , ScottyM
  , Process (..)
  , Route (..)
  , CustomWebServer (..)
  , EnvR, EnvW, EnvS, EnvRW, EnvRS, EnvWS
  , WebR, WebW, WebS, WebRW, WebRS, WebWS
  , ScottyR, ScottyW, ScottyS, ScottyRW, ScottyRS, ScottyWS
  , ProcessR, ProcessW, ProcessS, ProcessRW, ProcessRS, ProcessWS
  , RouteR, RouteW, RouteS, RouteRW, RouteRS, RouteWS
  , CustomWebServerR, CustomWebServerW, CustomWebServerS
  , CustomWebServerRW, CustomWebServerRS, CustomWebServerWS
  ) where

import           Control.Monad.RWS   (RWST (..))
import           Data.Text           as T (Text)
import           Network.Wai         (Response)
import           Web.Scotty.Trans    (ActionT, RoutePattern, ScottyT)
import           Web.Template.Except (Except)

-- | Alias for UserId.
type UserId = T.Text

-- | Alias for Port.
type Port = Int

-- | Alias for environment.
type Env r w s = RWST r w s IO

-- | Alias for Web monad. Incapsulates 'Web.Scotty.Trans.ActionT'.
type WebM r w s a = ActionT Except (Env r w s) a

-- | Alias for Scotty monad. Encapsulates 'Web.Scotty.Trans.ScottyT'
type ScottyM r w s a = ScottyT Except (Env r w s) a

-- | 'Process' encapsulates what we what to do inside 'Route'.
--   If your need to check authorization then use 'AuthProcess' constructor.
data Process r w s = Process (WebM r w s ())
                   | AuthProcess (UserId -> WebM r w s ())

-- | 'Route' include every needed information to make some stuff with request. It includes:
-- * environment @env@ that we can store and use (for example, connections for databases);
-- * method (like POST or GET);
-- * version of path (it should be like `/v{Integer}/`);
-- * path (just name of path);
-- * process (what should we do with request).
data Route r w s = Route { method  :: RoutePattern -> WebM r w s () -> ScottyT Except (Env r w s) ()
                         , version :: Int
                         , path    :: String
                         , process :: Process r w s
                         }

-- | Contains environment and processing routes.
data CustomWebServer r w s = CustomWebServer { readerEnv :: r
                                             , writerEnv :: w
                                             , stateEnv  :: s
                                             , handleLog :: IO (Response, w) -> IO Response
                                             , routes    :: [Route r w s]
                                             }

-----------------------------------------------------------------------------------------------------
-- DEFAULT TYPES --
-----------------------------------------------------------------------------------------------------

type EnvR r    = Env r () ()

type EnvW w    = Env () w ()

type EnvS s    = Env () () s

type EnvRW r w = Env r w ()

type EnvRS r s = Env r () s

type EnvWS w s = Env () w s

---------------------------------------------------

type WebR r a    = WebM r () () a

type WebW w a    = WebM () w () a

type WebS s a    = WebM () () s a

type WebRW r w a = WebM r w () a

type WebRS r s a = WebM r () s a

type WebWS w s a = WebM () w s a

---------------------------------------------------

type ScottyR r a    = ScottyM r () () a

type ScottyW w a    = ScottyM () w () a

type ScottyS s a    = ScottyM () () s a

type ScottyRW r w a = ScottyM r w () a

type ScottyRS r s a = ScottyM r () s a

type ScottyWS w s a = ScottyM () w s a

---------------------------------------------------

type ProcessR r    = Process r () ()

type ProcessW w    = Process () w ()

type ProcessS s    = Process () () s

type ProcessRW r w = Process r w ()

type ProcessRS r s = Process r () s

type ProcessWS w s = Process () w s

---------------------------------------------------

type RouteR r    = Route r () ()

type RouteW w    = Route () w ()

type RouteS s    = Route () () s

type RouteRW r w = Route r w ()

type RouteRS r s = Route r () s

type RouteWS w s = Route () w s

---------------------------------------------------

type CustomWebServerR r    = CustomWebServer r () ()

type CustomWebServerW w    = CustomWebServer () w ()

type CustomWebServerS s    = CustomWebServer () () s

type CustomWebServerRW r w = CustomWebServer r w ()

type CustomWebServerRS r s = CustomWebServer r () s

type CustomWebServerWS w s = CustomWebServer () w s

---------------------------------------------------
