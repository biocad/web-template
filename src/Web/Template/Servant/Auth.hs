module Web.Template.Servant.Auth
  where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import Control.Lens  (at, (.~), (?~))
import Data.Function ((&))
import Data.Functor  ((<&>))
import Data.Proxy    (Proxy (..))
import Data.Text     (Text)
import GHC.Generics  (Generic)

import Data.OpenApi.Internal     (ApiKeyLocation (..), ApiKeyParams (..), SecurityRequirement (..),
                                  SecurityScheme (..), SecuritySchemeType (..))
import Data.OpenApi.Lens         (components, description, security, securitySchemes)
import Data.OpenApi.Operation    (allOperations, setResponse)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai               (requestHeaders)
import Servant.API               ((:>))
import Servant.OpenApi           (HasOpenApi (..))
import Servant.Server            (HasServer (..), ServerError (..), err401)
import Servant.Server.Internal   (addAuthCheck, delayedFailFatal, withRequest)
import Web.Cookie                (parseCookiesText)

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
          maybe (delayedFailFatal err) return $
            lookup "cookie" (requestHeaders req)
              <&> parseCookiesText
              >>= lookup "id"
              <&> UserId
    where
      err = err401
        { errBody = "{\"error\": \"Authorization failed\"}"
        , errHeaders = [(hContentType, "application/json")]
        }

instance HasOpenApi api => HasOpenApi (CbdAuth :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & components . securitySchemes . at "cbdCookie" ?~ idCookie
    & allOperations . security .~ [SecurityRequirement $ mempty & at "cbdCookie" ?~ []]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idCookie = SecurityScheme
        (SecuritySchemeApiKey (ApiKeyParams "id" ApiKeyCookie))
        (Just "`id` cookie")
