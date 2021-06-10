{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Aeson                      (encode)
import Data.OpenApi                    (OpenApi)
import Data.Proxy                      (Proxy (..))
import Data.Text                       (Text)
import Servant                         (Description, Get, Handler, JSON, PlainText, Post, ReqBody,
                                        Summary, (:<|>) (..), (:>))
import Servant.OpenApi                 (toOpenApi)
import Servant.Server.Internal.Context (Context (..))

import Web.Template.Servant (OIDCAuth, OIDCConfig (..), Permit, SwaggerSchemaUI, UserId (..),
                             Version, defaultOIDCCfg, runServantServerWithContext,
                             swaggerSchemaUIServer)
import Web.Template.Wai     (defaultHandleLog, defaultHeaderCORS)

type API = Version "1" :>
  ( Summary "ping route" :> Description "Returns pong" :> "ping" :> Get '[PlainText] Text
  :<|> OIDCAuth :>
    ( Summary "hello route" :> Description "Returns hello + user id" :> "hello" :> Get '[PlainText] Text
    :<|> Permit '["set role here", "or here"] :> "post" :> ReqBody '[JSON] Int :> Post '[JSON] Text
    )
  )

pingH :: Handler Text
pingH = return "pong!"

helloH :: UserId -> Handler Text
helloH (UserId userId) = return $ "Hello " <> userId

postH :: UserId -> Int -> Handler Text
postH _ _ = return "Foo"

swagger :: OpenApi
swagger = toOpenApi @API Proxy

main :: IO ()
main = do
    print $ encode swagger
    cfg <- defaultOIDCCfg
    runServantServerWithContext @(SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> API)
        id
        (defaultHeaderCORS . defaultHandleLog)
        5000
        (cfg {oidcIssuer = uri, oidcClientId = cId} :. EmptyContext )
        $ swaggerSchemaUIServer swagger :<|> (pingH :<|> (\userId -> helloH userId :<|> postH userId))
  where
    uri = error "set uri here"
    cId = error "set client id here"
