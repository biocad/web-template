{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Proxy               (Proxy (..))
import Data.Swagger             (Swagger)
import Data.Text                (Text)
import Network.Wai.Handler.Warp (runSettings)
import Servant                  ((:<|>) (..), (:>), Description, Get, Handler, PlainText, Summary,
                                 serve)
import Servant.Swagger          (toSwagger)
import Servant.Swagger.UI       (SwaggerSchemaUI, swaggerSchemaUIServer)

import Web.Template.Servant.API  (Version)
import Web.Template.Servant.Auth (CbdAuth, UserId (..))
import Web.Template.Wai          (defaultHandleLog, defaultHeaderCORS, warpSettings)

type API = Version "1" :>
  ( Summary "ping route" :> Description "Returns pong" :> "ping" :> Get '[PlainText] Text
  :<|> CbdAuth :>
    ( Summary "hello route" :> Description "Returns hello + user id" :> "hello" :> Get '[PlainText] Text
    )
  )

pingH :: Handler Text
pingH = return "pong!"

helloH :: UserId -> Handler Text
helloH (UserId userId) = return $ "Hello " <> userId

swagger :: Swagger
swagger = toSwagger @API Proxy

main :: IO ()
main =
  runSettings (warpSettings 5000 id)
    $ defaultHandleLog
    $ defaultHeaderCORS
    $ serve @(SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> API) Proxy
    $ swaggerSchemaUIServer swagger :<|> pingH :<|> helloH
