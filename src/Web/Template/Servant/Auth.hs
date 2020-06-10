module Web.Template.Servant.Auth
  where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import Control.Lens  (at, (.~), (?~))
import Data.Function ((&))
import Data.Functor  ((<&>))
import Data.Proxy    (Proxy (..))
import Data.Text     (Text)
import GHC.Generics  (Generic)

import Data.Swagger.Internal   (ApiKeyLocation (..), ApiKeyParams (..), SecurityRequirement (..),
                                SecurityScheme (..), SecuritySchemeType (..))
import Data.Swagger.Lens       (description, security, securityDefinitions)
import Data.Swagger.Operation  (allOperations, setResponse)
import Network.Wai             (requestHeaders)
import Servant.API             ((:>))
import Servant.Server          (HasServer (..), ServerError (..), err401)
import Servant.Server.Internal (addAuthCheck, delayedFailFatal, withRequest)
import Servant.Swagger         (HasSwagger (..))
import Web.Cookie              (parseCookiesText)

-- | Add authenthication via @id@ Cookie.
--
-- Usage:
--
-- > type API = CbdAuth :> (....)
--
-- Handlers will get an 'UserId' argument.
data CbdAuth

newtype UserId = UserId { getUserId :: Text }
  deriving (Eq, Show, Generic)

instance HasServer api context => HasServer (CbdAuth :> api) context where
  type ServerT (CbdAuth :> api) m = UserId -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt . s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck sub
      $ withRequest $ \req ->
          -- TODO json error
          maybe (delayedFailFatal $ err401 { errBody = "Authorization failed" }) return $
            lookup "cookie" (requestHeaders req)
              <&> parseCookiesText
              >>= lookup "id"
              <&> UserId

-- FIXME swagger does not support describing cookies at all
-- Only OpenAPI 3.0 does
instance HasSwagger api => HasSwagger (CbdAuth :> api) where
  toSwagger _ = toSwagger @api Proxy
    & securityDefinitions . at "cbdCookie" ?~ idCookie
    & allOperations . security .~ [SecurityRequirement $ mempty & at "cbdCookie" ?~ []]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idCookie = SecurityScheme
        (SecuritySchemeApiKey (ApiKeyParams "Cookie" ApiKeyHeader))
        (Just "`id` cookie")
