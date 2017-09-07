{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Template.Server
  ( CustomWebServer (..), Process (..), Route (..)
  , runWebServerConf, runWebServer
  ) where

import           Control.Monad.Reader
import           Data.String                          (fromString)
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Text.Lazy                       as TL (Text, toStrict)
import           Network.HTTP.Types.Status            (status401, status405)
import           Network.Wai.Handler.Warp             (defaultSettings,
                                                       exceptionResponseForDebug,
                                                       setOnExceptionResponse,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Options.Generic                      (getRecord)
import           Web.Cookie                           (parseCookiesText)
import           Web.Scotty.Trans                     (Options (..),
                                                       RoutePattern, ScottyT,
                                                       defaultHandler, header,
                                                       json, middleware, param,
                                                       scottyOptsT, status)
import           Web.Template.Except                  (Except, JsonError (..),
                                                       handleEx)
import           Web.Template.Types                   (ServerConfig (..), ScottyM,
                                                       UserId, WebM)



data Process s = Process (WebM s ())
               | AuthProcess (UserId -> WebM s ())

data Route s = Route { method :: RoutePattern -> WebM s () -> ScottyT Except (ReaderT s IO) ()
                     , version :: Int
                     , path :: String
                     , process :: Process s
                     }

data CustomWebServer s = CustomWebServer { initialState :: s
                                         , routes       :: [Route s]
                                         }

runWebServerConf :: ServerConfig -> CustomWebServer s -> IO ()
runWebServerConf conf CustomWebServer{..} = scottyOptsT (scottyOpts conf) (`runReaderT` initialState) $ do
    middleware logStdoutDev
    defaultHandler handleEx
    mapM_ runRoute routes

runWebServer :: CustomWebServer s -> IO ()
runWebServer cws = do
    conf <- getRecord "Web template"
    runWebServerConf conf cws

runRoute :: Route s -> ScottyM s ()
runRoute Route{..} = method (fromString $ "/:version" ++ path) (checkVersion version . auth $ process)


scottyOpts :: ServerConfig -> Options
scottyOpts ServerConfig{..} = Options 1 warpSettings
  where warpSettings = setOnExceptionResponse exceptionResponseForDebug .
                       setPort port $ defaultSettings

auth :: Process s -> WebM s ()
auth (Process p) = p
auth (AuthProcess p) = do
    cookiesM <- header "Cookie"
    let idMaybe = cookiesM >>= getIdFromCookies
    case idMaybe of
        Just id' -> p id'
        Nothing -> do status status401
                      json . JsonError $ "Authorization failed"

checkVersion :: Int -> WebM s () -> WebM s ()
checkVersion version route = do
    versionPath <- param "version" :: WebM s String
    if "v" ++ show version == versionPath
      then route
      else do status status405
              json . JsonError $  "Server API version: " ++ show version ++ "; got version: " ++ versionPath

getIdFromCookies :: TL.Text -> Maybe UserId
getIdFromCookies cookies = lookup "id" $ parseCookiesText $ encodeUtf8 $ toStrict cookies
