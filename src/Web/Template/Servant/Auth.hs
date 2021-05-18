module Web.Template.Servant.Auth
  where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import           Control.Lens           (At (at), _Just, (&), (.~), (<&>), (?~), (^?!))
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef             (writeIORef)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Vault.Lazy        as V
import           GHC.Generics           (Generic)
import           Web.Template.Log       (pTokenVaultKey, tokenVaultKey, userIdVaultKey)

import           Crypto.JWT                     (JWTError, decodeCompact,
                                                 defaultJWTValidationSettings, unregisteredClaims,
                                                 verifyClaims)
import           Data.Aeson                     (Value (..))
import           Data.ByteString                (stripPrefix)
import qualified Data.ByteString.Lazy           as LB
import           Data.Maybe                     (fromMaybe)
import           Data.OpenApi.Internal          (ApiKeyLocation (..), ApiKeyParams (..),
                                                 SecurityRequirement (..), SecurityScheme (..),
                                                 SecuritySchemeType (..), HttpSchemeType (..))
import           Data.OpenApi.Lens              (components, description, security, securitySchemes)
import           Data.OpenApi.Operation         (allOperations, setResponse)
import           Network.HTTP.Client            (httpLbs)
import           Network.HTTP.Types.Header      (hContentType)
import           Network.URI                    (parseURI)
import           Network.Wai                    (requestHeaders, vault)
import           OpenID.Connect.Client.Provider (Discovery (Discovery, jwksUri), keysFromDiscovery)
import qualified OpenID.Connect.Client.Provider as OIDC
import           Servant.API                    ((:>))
import           Servant.OpenApi                (HasOpenApi (..))
import           Servant.Server                 (HasServer (..), ServerError (..), err401, HasContextEntry (getContextEntry))
import           Servant.Server.Internal        (addAuthCheck, delayedFailFatal, withRequest)
import           Web.Cookie                     (parseCookiesText)

import Web.Template.Types (JWKSURI (..), TPAuthManager (..))

-- | Add authenthication via @id@ Cookie.
--
-- Usage:
--
-- > type API = CbdAuth :> (....)
--
-- Handlers will get an 'UserId' argument.
data CbdAuth

newtype UserId
  = UserId { getUserId :: Text }
  deriving (Eq, Show, Generic)

instance HasServer api context => HasServer (CbdAuth :> api) context where
  type ServerT (CbdAuth :> api) m = UserId -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt . s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck sub
      $ withRequest $ \req -> do
          let
            mUserId =
              lookup "cookie" (requestHeaders req)
                <&> parseCookiesText
                >>= lookup "id"
          case mUserId of
            Nothing -> delayedFailFatal err
            Just uid -> do
                -- Try to store user id in the vault, to be used by logging middleware later.
                let mUserIdRef = V.lookup userIdVaultKey $ vault req
                case mUserIdRef of
                  Nothing  -> return ()
                  Just ref -> liftIO $ writeIORef ref $ Just uid
                return $ UserId uid
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

data OIDCAuth

instance ( HasServer api context
         , HasContextEntry context TPAuthManager
         , HasContextEntry context JWKSURI
         ) => HasServer (OIDCAuth :> api) context where
  type ServerT (OIDCAuth :> api) m = UserId -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt . s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck sub
      $ withRequest $ \req -> do

          case (getHeader req, getHeader req >>= getToken) of
            (Just token, Just jws) -> do
              let mgr = getManager $ getContextEntry @_ @TPAuthManager context
              let jwksURI = fromMaybe (error "incorrect jwks uri") $
                    parseURI $ getURI $
                    getContextEntry @_ @JWKSURI context
              keysResp <- liftIO $
                keysFromDiscovery (https mgr) $ Discovery {jwksUri = OIDC.URI jwksURI}

              case keysResp of
                Left _error -> delayedFailFatal err
                Right (jwkSet, mbKeysExp) -> do
                  let audCheck = const True
                  res <- liftIO $
                    runExceptT $
                      verifyClaims @_ @_ @JWTError (defaultJWTValidationSettings audCheck) jwkSet jws
                  case res of
                    Left _error -> delayedFailFatal err
                    Right claims -> do
                      let String uid = claims
                            ^?! unregisteredClaims
                            .at "object_guid"
                            ._Just
                      let mUserIdRef = V.lookup userIdVaultKey $ vault req
                      let mTokenRef = V.lookup tokenVaultKey $ vault req
                      let mPTokenRef = V.lookup pTokenVaultKey $ vault req

                      case mUserIdRef of
                        Nothing  -> return ()
                        Just ref -> liftIO $ writeIORef ref $ Just uid

                      case mTokenRef of
                        Nothing  -> return ()
                        Just ref -> liftIO $ writeIORef ref $ Just $ decodeUtf8 token

                      case mPTokenRef of
                        Nothing  -> return ()
                        Just ref -> liftIO $ writeIORef ref $ Just claims

                      return $ UserId uid
            _ -> delayedFailFatal err
    where
      https mgr = (`httpLbs` mgr)

      getHeader r = lookup "Authorization" (requestHeaders r) >>= stripPrefix "Bearer "

      getToken = either (const Nothing) Just . decodeCompact @_ @JWTError . LB.fromStrict

      err = err401
        { errBody = "{\"error\": \"Authorization failed\"}"
        , errHeaders = [(hContentType, "application/json")]
        }

instance HasOpenApi api => HasOpenApi (OIDCAuth :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & components . securitySchemes . at "cbdJwt" ?~ idJWT
    & allOperations . security .~ [SecurityRequirement $ mempty & at "cbdJWT" ?~ []]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idJWT = SecurityScheme
        (SecuritySchemeHttp $ HttpSchemeBearer $ Just "jwt")
        (Just "jwt token")
