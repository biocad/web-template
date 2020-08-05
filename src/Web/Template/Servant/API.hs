module Web.Template.Servant.API where

import Control.Lens  ((?~))
import Data.Function ((&))
import Data.OpenApi  (applyTags, description)
import Data.Proxy    (Proxy (..))
import Data.String   (fromString)
import Data.Text     (pack)
import GHC.TypeLits  (AppendSymbol, KnownSymbol, Symbol, symbolVal)

import Servant         ((:>), HasServer (..))
import Servant.OpenApi (HasOpenApi (..))

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
