{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Aeson         (encode)
import Data.OpenApi       (OpenApi)
import Data.Proxy         (Proxy (..))
import Data.Text          (Text)
import Servant            (Description, Get, Handler, JSON, PlainText, Post, ReqBody, Summary,
                           (:<|>) (..), (:>))
import Servant.OpenAPI.UI (OpenApiSchemaUI, openapiSchemaUIServer)
import Servant.OpenApi    (toOpenApi)

import Web.Template.Servant (CbdAuth, UserId (..), Version, runServantServer)

type API = Version "1" :>
  ( Summary "ping route" :> Description "Returns pong" :> "ping" :> Get '[PlainText] Text
  :<|> CbdAuth :>
    ( Summary "hello route" :> Description "Returns hello + user id" :> "hello" :> Get '[PlainText] Text
    :<|> "post" :> ReqBody '[JSON] Int :> Post '[JSON] Text
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

  runServantServer @(OpenApiSchemaUI "swagger-ui" "swagger.json" :<|> API) 5000
    $ openapiSchemaUIServer swagger :<|> (pingH :<|> (\userId -> helloH userId :<|> postH userId))