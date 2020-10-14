module Web.Template.Servant.API where

import Control.Lens  ((?~))
import Data.Function ((&))
import Data.OpenApi  (applyTags, description)
import Data.Proxy    (Proxy (..))
import Data.String   (fromString)
import Data.Text     (pack)
import GHC.TypeLits  (AppendSymbol, KnownSymbol, Symbol, symbolVal)

import Control.Monad.IO.Class       (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Network.Wai                  (Request, Response, ResponseReceived)
import Servant                      (HasServer (..), Raw, (:>))
import Servant.OpenApi              (HasOpenApi (..))
import Servant.Server.Internal      (RouteResult (..), Router' (..), runDelayed, runHandler)

-- | Prepend version to every sub-route.
--
-- > type API = Version "3" :> ("route1" :<|> "route2")
type Version (v :: Symbol) = AppendSymbol "v" v

-- | Mark sub-api with a Swagger tag with description.
--
-- > type API
-- >   =    (Tag "foo" "Some Foo routes" :> ("foo1" :<|> "foo2"))
-- >   :<|> (Tag "bar" "Some Bar routes" :> ("bar1" :<|> "bar2"))
data Tag (tag :: Symbol) (descr :: Symbol)

instance HasServer api context => HasServer (Tag tag descr :> api) context where
  type ServerT (Tag tag descr :> api) m = ServerT api m

  route _ = route @api Proxy
  hoistServerWithContext _ = hoistServerWithContext @api Proxy

instance (KnownSymbol tag, KnownSymbol descr, HasOpenApi api) => HasOpenApi (Tag tag descr :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & applyTags [fromString (symbolVal @tag Proxy) & description ?~ pack (symbolVal @descr Proxy)]

-- | As 'Raw', but with access to the custom monad @m@.
--
-- See <https://github.com/haskell-servant/servant/pull/1349>.
data RawM

instance HasServer RawM ctx where
  type ServerT RawM m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

  hoistServerWithContext _ _ nt m = \request respond -> nt $ m request respond

  route _ _ dApp = RawRouter $ \env request respond -> runResourceT $ do
    r <- runDelayed dApp env request
    liftIO $ case r of
      Fail a -> respond $ Fail a
      FailFatal e -> respond $ FailFatal e
      Route appH -> do
        r' <- runHandler $ appH request (respond . Route)
        -- appH may return result with 'Right' _only_ by calling smth like @liftIO . respond@,
        -- so in case of 'Left' we may suppose that 'respond' was never called.
        case r' of
          Left e  -> respond $ FailFatal e
          Right x -> return x

instance HasOpenApi RawM where
  toOpenApi _ = toOpenApi @Raw Proxy
