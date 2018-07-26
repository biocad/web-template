{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Web.Template.Server
  ( UserId, Port, Env, WebM, ScottyM
  , CustomWebServer (..), Process (..), Route (..)
  , runWebServer
  ) where

import           Control.Monad.Reader                 (ReaderT (..), runReaderT)
import           Data.String                          (fromString)
import           Data.Text                            as T (Text)
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Text.Lazy                       as TL (Text, toStrict)
import           Network.HTTP.Types.Status            (status401, status405)
import           Network.Wai.Handler.Warp             (defaultSettings,
                                                       exceptionResponseForDebug,
                                                       setOnExceptionResponse,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Cookie                           (parseCookiesText)
import           Web.Scotty.Trans                     (ActionT, Options (..),
                                                       RoutePattern, ScottyT,
                                                       defaultHandler, header,
                                                       json, middleware, param,
                                                       scottyOptsT, status)
import           Web.Template.Except                  (Except,
                                                       JsonWebError (..),
                                                       handleEx)

-- | Alias for UserId.
type UserId = T.Text

-- | Alias for Port.
type Port = Int

-- | Alias for environment.
type Env env = ReaderT env IO

-- | Alias for Web monad. Incapsulates 'Web.Scotty.Trans.ActionT'.
type WebM env a = ActionT Except (Env env) a

-- | Alias for Scotty monad. Encapsulates 'Web.Scotty.Trans.ScottyT'
type ScottyM env a = ScottyT Except (Env env) a

-- | 'Process' encapsulates what we what to do inside 'Route'.
--   If your need to check authorization then use 'AuthProcess' constructor.
data Process s = Process (WebM s ())
               | AuthProcess (UserId -> WebM s ())

-- | 'Route' include every needed information to make some stuff with request. It includes:
-- * environment @env@ that we can store and use (for example, connections for databases);
-- * method (like POST or GET);
-- * version of path (it should be like `/v{Integer}/`);
-- * path (just name of path);
-- * process (what should we do with request).
data Route env = Route { method  :: RoutePattern -> WebM env () -> ScottyT Except (Env env) ()
                       , version :: Int
                       , path    :: String
                       , process :: Process env
                       }

-- | Contains environment and processing routes.
data CustomWebServer env = CustomWebServer { environment :: env
                                           , routes      :: [Route env]
                                           }

-- | For given port and server settings run the server.
runWebServer :: Port -> CustomWebServer env -> IO ()
runWebServer port CustomWebServer{..} = scottyOptsT (scottyOpts port) (`runReaderT` environment) $ do
    middleware logStdout
    defaultHandler handleEx
    _ <- mapM runRoute routes
    pure ()

runRoute :: Route env -> ScottyM env ()
runRoute Route{..} = method (fromString $ "/:version" ++ path) (checkVersion version . auth $ process)

scottyOpts :: Port -> Options
scottyOpts port = Options 1 warpSettings
  where warpSettings = setOnExceptionResponse exceptionResponseForDebug .
                       setPort port $ defaultSettings

auth :: Process env -> WebM env ()
auth (Process p) = p
auth (AuthProcess p) = do
    cookiesM <- header "Cookie"
    let idMaybe = cookiesM >>= getIdFromCookies
    case idMaybe of
        Just id' -> p id'
        Nothing -> do status status401
                      json . JsonWebError $ "Authorization failed"

checkVersion :: Int -> WebM env () -> WebM env ()
checkVersion version route = do
    versionPath <- param "version" :: WebM s String
    if "v" ++ show version == versionPath
      then route
      else do status status405
              json . JsonWebError $ "Server API version: " ++ show version ++ "; got version: " ++ versionPath

getIdFromCookies :: TL.Text -> Maybe UserId
getIdFromCookies cookies = lookup "id" $ parseCookiesText $ encodeUtf8 $ toStrict cookies
