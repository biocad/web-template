{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Web.Template.Servant.Auth
  ( CbdAuth
  , OIDCAuth
  , UserId (..)
  , OIDCConfig (..)
  , defaultOIDCCfg
  , oidcCfgWithManager
  , OIDCUser (..)
  , Permit
  , swaggerSchemaUIBCDServer
  ) where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import           Control.Applicative    ((<|>))
import           Control.Lens           (at, ix, (&), (.~), (<&>), (?~), (^..), (^?))
import           Control.Monad.Except   (runExceptT, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef             (readIORef, writeIORef)
import           Data.Maybe             (catMaybes)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text, intercalate, pack)
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Vault.Lazy        as V
import           GHC.Generics           (Generic)
import           GHC.TypeLits           (KnownSymbol, Symbol, symbolVal)

import           Crypto.JOSE.JWK                (JWKSet)
import           Crypto.JWT                     (ClaimsSet, JWTError, JWTValidationSettings,
                                                 SignedJWT, audiencePredicate, decodeCompact,
                                                 defaultJWTValidationSettings, issuerPredicate,
                                                 string, unregisteredClaims, uri, verifyClaims)
import           Data.Aeson                     (Value)
import           Data.Aeson.Lens                (AsPrimitive (_String), key, values)
import           Data.ByteString                (ByteString, stripPrefix)
import qualified Data.ByteString.Lazy           as LB
import           Data.Cache                     (Cache)
import qualified Data.Cache                     as Cache
import           Data.OpenApi                   (OpenApi, URL (..))
import           Data.OpenApi.Internal          (ApiKeyLocation (..), ApiKeyParams (..),
                                                 HttpSchemeType (..), SecurityRequirement (..),
                                                 SecurityScheme (..), SecuritySchemeType (..))
import           Data.OpenApi.Lens              (components, description, security, securitySchemes)
import           Data.OpenApi.Operation         (allOperations, setResponse)
import qualified Data.Text                      as T
import           Data.Time.Clock                (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime,
                                                 getCurrentTime, nominalDiffTimeToSeconds)
import           FileEmbedLzma                  (embedText)
import           Network.HTTP.Client            (Manager, httpLbs)
import           Network.HTTP.Client.TLS        (newTlsManager)
import           Network.HTTP.Types.Header      (hContentType)
import           Network.URI                    (URI (..))
import           Network.Wai                    (Request, requestHeaders, vault)
import           OpenID.Connect.Client.Provider (Discovery, discovery, keysFromDiscovery)
import           Servant.API                    ((:>))
import           Servant.OpenApi                (HasOpenApi (..))
import           Servant.Server                 (HasContextEntry (getContextEntry), HasServer (..),
                                                 ServerError (..), err401, err403, err500)
import           Servant.Server.Internal        (DelayedIO, addAuthCheck, delayedFailFatal,
                                                 withRequest)
import           Servant.Swagger.UI             (SwaggerSchemaUI', swaggerUiFiles)
import           Servant.Swagger.UI.Core        (swaggerSchemaUIServerImpl)
import           System.Clock                   (TimeSpec (..))
import           Web.Cookie                     (parseCookiesText)

import System.BCD.Log   (Level (..), log')
import Web.Template.Log (pTokenVaultKey, tokenVaultKey, userIdVaultKey)

-- | Adds authenthication via @id@ Cookie.
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
            Nothing -> unauth401
            Just uid -> do
                -- Try to store user id in the vault, to be used by logging middleware later.
                let mUserIdRef = V.lookup userIdVaultKey $ vault req
                case mUserIdRef of
                  Nothing  -> return ()
                  Just ref -> liftIO $ writeIORef ref $ Just uid
                return $ UserId uid

instance HasOpenApi api => HasOpenApi (CbdAuth :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & components . securitySchemes . at "cbdCookie" ?~ idCookie
    & allOperations . security .~ [SecurityRequirement $ mempty & at "cbdCookie" ?~ []]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idCookie = SecurityScheme
        (SecuritySchemeApiKey (ApiKeyParams "id" ApiKeyCookie))
        (Just "`id` cookie")

-- | Adds authenthication via jwt
--
-- Usage:
--
-- > type API = OIDCAuth :> (....)
--
-- Takes token from 'Authorization' header.
--
-- Handlers will get an 'UserId' argument.
--
-- Stores token and claims in vault.
data OIDCAuth


data OIDCUser
  = OIDCUser
      { oidcUserId      :: UserId
      , oidcAccessToken :: Text
      , oidcParsedToken :: ClaimsSet
      }
  deriving (Eq, Show, Generic)

-- | Info needed for OIDC authorization & key cache
data OIDCConfig
  = OIDCConfig
      { oidcManager           :: Manager
        -- ^ https manager
      , oidcClientId          :: Text
        -- ^ audience
      , oidcIssuer            :: URI
        -- ^ discovery uri
      , oidcDiscoCache        :: Cache () Discovery
        -- ^ cache - storing discovery information
      , oidcKeyCache          :: Cache () JWKSet
        -- ^ cache - storing validation keys
      , oidcDefaultExpiration :: NominalDiffTime
        -- ^ Default expiration time for discovery document and JWKS
      , oidcAllowServiceToken :: Bool
        -- ^ Whether to accept service token (defined as "token without object_guid claim")
      }

defaultOIDCCfg :: MonadIO m => m OIDCConfig
defaultOIDCCfg = newTlsManager >>= oidcCfgWithManager

oidcCfgWithManager :: MonadIO m => Manager -> m OIDCConfig
oidcCfgWithManager mgr = do
  discoCache <- liftIO $ Cache.newCache $ Just 0
  keyCache <- liftIO $ Cache.newCache $ Just 0
  return $ OIDCConfig
    { oidcManager = mgr
    , oidcDiscoCache = discoCache
    , oidcKeyCache = keyCache
    , oidcIssuer = error "discovery uri not set"
    , oidcClientId = error "client id not set"
    , oidcDefaultExpiration = 10 * 60 -- 10 minutes
    , oidcAllowServiceToken = False
    }

instance ( HasServer api context
         , HasContextEntry context OIDCConfig
         ) => HasServer (OIDCAuth :> api) context where

  type ServerT (OIDCAuth :> api) m = OIDCUser -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt . s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck sub
      $ withRequest $ \req -> do

        token <- maybe unauth401 return $ getToken req

        jwt <- getJWT token

        let cfg = getContextEntry context

        disco <- getDisco cfg

        jwkSet <- getJWKSet cfg disco

        claims <- getClaims cfg jwt jwkSet

        let guid = claims ^? unregisteredClaims . ix "object_guid" . _String
        let username = claims ^? unregisteredClaims . ix "preferred_username" . _String

        uid <- maybe
          (die ERROR unauth401 ("No object_guid found" :: Text))
          return
          (guid <|> (if oidcAllowServiceToken cfg then username else Nothing))

        liftIO $ sequence_ $ catMaybes
          [ userIdVaultKey <?> req <&> flip writeIORef (Just uid)
          , tokenVaultKey  <?> req <&> flip writeIORef (Just $ decodeUtf8 token)
          , pTokenVaultKey <?> req <&> flip writeIORef (Just claims)
          ]

        return OIDCUser
          { oidcUserId = UserId uid
          , oidcAccessToken = decodeUtf8 token
          , oidcParsedToken = claims
          }
    where
      https mgr = (`httpLbs` mgr)

      die :: Show err => Level -> DelayedIO b -> err -> DelayedIO b
      die lvl fin err = liftIO (log' lvl ("web-template" :: Text) $ show err) >> fin

      getToken :: Request -> Maybe ByteString
      getToken r = lookup "Authorization" (requestHeaders r) >>= stripPrefix "Bearer "

      expiration :: UTCTime -> Maybe UTCTime -> NominalDiffTime -> Maybe TimeSpec
      expiration now ex defaultExp = diffTime
          <$> (ex <|> pure (addUTCTime defaultExp now))
              -- If expiration is not set by OIDC provider, cache data for some
              -- default amount of time, to avoid too many requests.
          <*> pure now
        where
          tTreshold = 60 -- consider token expired 'tTreshold' seconds earlier

          diffTime :: UTCTime -> UTCTime -> TimeSpec
          diffTime from to = let
              diff = diffUTCTime from to - tTreshold
            in max
                TimeSpec {sec = 0, nsec = 0}
                TimeSpec {sec = floor $ nominalDiffTimeToSeconds diff, nsec = 0}

      getJWT :: ByteString -> DelayedIO SignedJWT
      getJWT = either (die WARNING unauth401) return . decodeToken
        where
            decodeToken = decodeCompact @_ @JWTError . LB.fromStrict

      getDisco :: OIDCConfig -> DelayedIO Discovery
      getDisco OIDCConfig {..} = liftIO (Cache.lookup oidcDiscoCache ())
          >>= maybe
            fetchDisco
            return
        where
          fetchDisco = liftIO (discovery (https oidcManager) (appWellKnown oidcIssuer))
              >>= either
                (die ERROR unauth500)
                (uncurry discoSuccess)
            where
              discoSuccess disco mbDiscoExp = liftIO $ do
                now <- getCurrentTime
                Cache.insert' oidcDiscoCache (expiration now mbDiscoExp oidcDefaultExpiration) () disco
                return disco

      getJWKSet :: OIDCConfig -> Discovery -> DelayedIO JWKSet
      getJWKSet OIDCConfig {..} disco = liftIO (Cache.lookup oidcKeyCache ())
          >>= maybe
            fetchKeys
            return
        where
          fetchKeys = liftIO (keysFromDiscovery (https oidcManager) disco)
              >>= either
                (die ERROR unauth500)
                (uncurry keysSuccess)
            where
              keysSuccess jwkSet mbKeysExp = liftIO $ do
                  now <- getCurrentTime
                  Cache.insert' oidcKeyCache (expiration now mbKeysExp oidcDefaultExpiration) () jwkSet
                  return jwkSet

      getClaims :: OIDCConfig -> SignedJWT -> JWKSet -> DelayedIO ClaimsSet
      getClaims OIDCConfig {..} jwt jwkSet = liftIO
          (runExceptT $
            verifyClaims @_ @_ @JWTError (jwtValidation oidcIssuer oidcClientId) jwkSet jwt
          ) >>= either
            (die ERROR unauth401)
            return
        where
          jwtValidation :: URI -> Text -> JWTValidationSettings
          jwtValidation issuer audience = defaultJWTValidationSettings (const True)
            & issuerPredicate .~ (\iss -> iss ^? uri == Just issuer)
            & audiencePredicate .~ (\aud -> aud ^? string == Just audience)

instance HasOpenApi api => HasOpenApi (OIDCAuth :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & components . securitySchemes . at "cbdJWT" ?~ idJWT
    & components . securitySchemes . at "cbdOIDC" ?~ idOIDC
    & allOperations . security .~
        [ SecurityRequirement $ mempty & at "cbdJWT" ?~ []
        , SecurityRequirement $ mempty & at "cbdOIDC" ?~ []
        ]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idJWT = SecurityScheme
        (SecuritySchemeHttp $ HttpSchemeBearer $ Just "jwt")
        (Just "jwt token")
      idOIDC = SecurityScheme
        -- It is expected that client will update this field in runtime before serving
        -- generated swagegr, as there is no easy way to pass it to this instance.
        (SecuritySchemeOpenIdConnect $ URL "NOT SET")
        (Just "BIOCAD's OpenID Connect auth")

-- | Adds authenthication via roles
--
-- Usage:
--
-- > type API = Permit '["user", "owner"] :> (....)
-- Route access permitted if user has at least 1 role from specified list
-- Takes user roles from vault (originated from jwt token)
data Permit (rs :: [Symbol])

instance ( HasServer api context
         , KnownSymbols roles
         , HasContextEntry context OIDCConfig
         ) => HasServer (Permit roles :> api) context where

  type ServerT (Permit roles :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck (const <$> sub)
      $ withRequest $ \req -> do
        pTokenRef <- maybe unauth401 return $ pTokenVaultKey <?> req

        claims <- liftIO (readIORef pTokenRef) >>= maybe unauth401 return

        let OIDCConfig {..} = getContextEntry context

        let haveRoles = claims
              ^.. unregisteredClaims
              . ix "resource_access"
              . key oidcClientId
              . key "roles"
              . values . _String

        unless (any (`elem` rolesNeeded) haveRoles)
          unauth403
    where
      rolesNeeded = symbolsVal (Proxy @roles)

instance ( HasOpenApi api
         , KnownSymbols roles
         ) => HasOpenApi (Permit roles :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & setResponse 403 (return $ mempty & description .~ descr)
    where
      descr = "Action not permitted. Allowed for: "
        <> intercalate ", " (symbolsVal (Proxy :: Proxy roles))

class KnownSymbols (rs :: [Symbol]) where
  symbolsVal :: p rs ->  [Text]

instance KnownSymbols '[] where
  symbolsVal _ = []

instance (KnownSymbol h, KnownSymbols t) => KnownSymbols (h ': t) where
  symbolsVal _ = pack (symbolVal (Proxy :: Proxy h)) : symbolsVal (Proxy :: Proxy t)

(<?>) :: V.Key a -> Request -> Maybe a
what <?> req = V.lookup what $ vault req

unauth401 :: DelayedIO a
unauth401 = delayedFailFatal $ err401
  { errBody = "{\"error\": \"Authorization failed\"}"
  , errHeaders = [(hContentType, "application/json")]
  }

unauth403 :: DelayedIO a
unauth403 = delayedFailFatal $ err403
  { errBody = "{\"error\": \"Action not permitted\"}"
  , errHeaders = [(hContentType, "application/json")]
  }

unauth500 :: DelayedIO a
unauth500 = delayedFailFatal $ err500
  { errBody = "{\"error\": \"Internal server error\"}"
  , errHeaders = [(hContentType, "application/json")]
  }

swaggerUiIndexBCDTemplate :: Text
swaggerUiIndexBCDTemplate = $(embedText "index.html.tmpl")

-- | Version of 'Servant.Swagger.UI.swaggerSchemaUIServer' that uses
-- our @index.html@ template to enable PKCE auth flow and prefill
-- OpenID client id.
swaggerSchemaUIBCDServer
  :: Monad m
  => ServerT api m ~ m Value
  => OIDCConfig
  -> OpenApi
  -> ServerT (SwaggerSchemaUI' dir api) m
swaggerSchemaUIBCDServer oidcConfig openapi =
  swaggerSchemaUIServerImpl swaggerUiIndexTemplateFilled swaggerUiFiles openapiWithOidcUrl
  where
    swaggerUiIndexTemplateFilled =
      T.replace "BIOCAD_OIDC_CLIENT_ID" (oidcClientId oidcConfig)
      swaggerUiIndexBCDTemplate
    openapiWithOidcUrl = openapi
      & components . securitySchemes . at "cbdOIDC" ?~
        SecurityScheme
          (SecuritySchemeOpenIdConnect $ URL $ T.pack $ show $ appWellKnown $ oidcIssuer oidcConfig)
          (Just "BIOCAD's OpenID Connect auth")

-- | Append OIDC's @.well-known/openid-configuration" part to the base of OIDC issuer URI.
appWellKnown :: URI -> URI
appWellKnown u@URI {..} = u {uriPath = uriPath <> "/.well-known/openid-configuration"}
