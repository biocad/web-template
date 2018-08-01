{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Web.Template.Server
  (
    runWebServer
  , defaultHandleLog
  ) where

import           Control.Monad.RWS                    (evalRWST)
import           Data.String                          (fromString)
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Text.Lazy                       as TL (Text, toStrict)
import           Network.HTTP.Types.Status            (status401, status405)
import           Network.Wai                          (Response)
import           Network.Wai.Handler.Warp             (defaultSettings,
                                                       exceptionResponseForDebug,
                                                       setOnExceptionResponse,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Cookie                           (parseCookiesText)
import           Web.Scotty.Trans                     (Options (..),
                                                       defaultHandler, header,
                                                       json, middleware, param,
                                                       scottyOptsT, status)
import           Web.Template.Except                  (JsonWebError (..),
                                                       handleEx)
import           Web.Template.Types

-- | For given port and server settings run the server.
runWebServer :: (Monoid w, Show w) => Port -> CustomWebServer r w s -> IO ()
runWebServer port CustomWebServer{..} = scottyOptsT (scottyOpts port) (handleLog .
                                                    (\rws -> evalRWST rws readerEnv stateEnv)) $ do
    middleware logStdout
    defaultHandler handleEx
    mapM_ runRoute routes

defaultHandleLog :: Show w => IO (Response, w) -> IO Response
defaultHandleLog = (print . snd <$>) >> (fst <$>)

runRoute :: Monoid w => Route r w s -> ScottyM r w s ()
runRoute Route{..} = method (fromString $ "/:version" ++ path) (checkVersion version . auth $ process)

scottyOpts :: Port -> Options
scottyOpts port = Options 1 warpSettings
  where warpSettings = setOnExceptionResponse exceptionResponseForDebug .
                       setPort port $ defaultSettings

auth :: Monoid w => Process r w s -> WebM r w s ()
auth (Process p) = p
auth (AuthProcess p) = do
    cookiesM <- header "Cookie"
    let idMaybe = cookiesM >>= getIdFromCookies
    case idMaybe of
        Just id' -> p id'
        Nothing -> do status status401
                      json . JsonWebError $ "Authorization failed"

checkVersion :: Monoid w => Int -> WebM r w s () -> WebM r w s ()
checkVersion version route = do
    versionPath <- param "version"
    if "v" ++ show version == versionPath
      then route
      else do status status405
              json . JsonWebError $ "Server API version: " ++ show version ++ "; got version: " ++ versionPath

getIdFromCookies :: TL.Text -> Maybe UserId
getIdFromCookies cookies = lookup "id" $ parseCookiesText $ encodeUtf8 $ toStrict cookies
