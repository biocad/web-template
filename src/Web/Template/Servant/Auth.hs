{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- unregisteredClaims is deprecated in jose, we don't want to fix that now.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Web.Template.Servant.Auth
  ( CbdAuth
  , OIDCAuth
  , UserId (..)
  , OIDCConfig (..)
  , defaultOIDCCfg
  , oidcCfgWithManager
  , OIDCUser (..)
  , authenticateOIDC
  , Permit
  , swaggerSchemaUIBCDServer
  ) where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import           Control.Applicative    ((<|>))
import           Control.Lens           (Iso', at, coerced, ix, (&), (.~), (<&>), (?~), (^..), (^?))
import           Control.Monad          (unless)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
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

#if MIN_VERSION_aeson(2, 0, 0)
import           Data.Aeson.Key                 (fromText)
#endif

import           Data.Aeson.Lens                (_String, key, values)
import           Data.ByteString                (ByteString, stripPrefix)
import qualified Data.ByteString.Lazy           as LB
import           Data.Cache                     (Cache)
import qualified Data.Cache                     as Cache
import           Data.OpenApi                   (Definitions, OpenApi, URL (..))
import           Data.OpenApi.Internal          (ApiKeyLocation (..), ApiKeyParams (..),
                                                 HttpSchemeType (..), SecurityDefinitions (..),
                                                 SecurityRequirement (..), SecurityScheme (..),
                                                 SecuritySchemeType (..))
import           Data.OpenApi.Lens              (components, description, security, securitySchemes)
import           Data.OpenApi.Operation         (allOperations, setResponse, setResponseWith)
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
import Web.Template.Log (pTokenVaultKey, rolesVaultKey, tokenVaultKey, userIdVaultKey)

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
    & components . securitySchemes . securityDefinitions . at "cbdCookie" ?~ idCookie
    & allOperations . security .~ [SecurityRequirement $ mempty & at "cbdCookie" ?~ []]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idCookie = SecurityScheme
        (SecuritySchemeApiKey (ApiKeyParams "id" ApiKeyCookie))
        (Just "`id` cookie")

-- | Adds authenthication via JWT.
--
-- Usage:
--
-- > type API = OIDCAuth :> (....)
--
-- Takes token from the @Authorization@ header.
-- Handlers will get an 'OIDCUser' argument.
--
-- See 'authenticateOIDC' for the full pipeline description and vault contract.
data OIDCAuth


data OIDCUser
  = OIDCUser
      { oidcUserId      :: UserId
      , oidcAccessToken :: Text
      , oidcParsedToken :: ClaimsSet
      , oidcRoles       :: [Text]
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

-- | Run the full OIDC authentication pipeline against a WAI 'Request'.
--
-- This is the core logic of 'OIDCAuth', exposed so that applications can
-- build custom Servant combinators that incorporate OIDC validation — for
-- example, a combinator that tries application-specific auth first and falls
-- back to OIDC — without duplicating this implementation.
--
-- === Pipeline
--
-- 1. Extract a @Bearer@ token from the @Authorization@ request header.
-- 2. Decode the raw bytes as a signed JWT.
-- 3. Fetch (and cache in 'oidcDiscoCache') the OIDC discovery document at
--    @\<oidcIssuer\>\/.well-known\/openid-configuration@.
-- 4. Fetch (and cache in 'oidcKeyCache') the JWK key set referenced by
--    the discovery document.
-- 5. Verify the JWT: check signature, @iss@ == 'oidcIssuer',
--    @aud@ == 'oidcClientId'.
-- 6. Extract the user identity from the @object_guid@ unregistered claim.
--    If absent and 'oidcAllowServiceToken' is 'True', falls back to
--    @preferred_username@.
-- 7. Populate the vault (see /Vault contract/ below).
--
-- === Vault contract
--
-- The WAI vault holds 'IORef' cells pre-inserted by the logging middleware.
-- Auth combinators signal identity by writing into these cells; they must
-- never insert new keys.  The relevant keys are defined in "Web.Template.Log".
--
-- __When delegating to 'authenticateOIDC':__ on 'Right' this function writes
-- all three keys listed below; the caller must not write them again.
-- On 'Left' the vault is left untouched.
--
-- __When taking a path that bypasses 'authenticateOIDC':__ the combinator is
-- responsible for populating the vault itself.
--
-- * 'rolesVaultKey'  — __required for 'Permit'__.  Write @Just roles@ where
--   @roles@ is the list of role strings the authenticated principal holds.
--   'Permit' returns 401 if the key is absent from the vault (logging
--   middleware not set up) or if the 'IORef' is still 'Nothing', and 403 if
--   none of the required roles are present.  Because 'Permit' reads only this
--   key, any combinator — OIDC or otherwise — can make 'Permit' work simply
--   by writing the right role list here.
-- * 'pTokenVaultKey' — write @Just claims@ to the 'IORef' for logging.
-- * 'userIdVaultKey' — write @Just uid@ so the logging middleware records the
--   authenticated user ID.
-- * 'tokenVaultKey'  — write @Just token@ so the logging middleware records
--   the raw credential.
--
-- === Errors
--
-- Returns @'Left' 'ServerError'@ (never throws) in the following cases:
--
-- [@401@] No @Authorization: Bearer \<token\>@ header present.
-- [@401@] The @Authorization@ value is not a valid signed JWT.
-- [@401@] JWT verification fails (signature, issuer, audience, or expiry).
-- [@401@] No @object_guid@ claim and service tokens are not allowed.
-- [@500@] OIDC discovery document or JWK set could not be fetched.
authenticateOIDC :: OIDCConfig -> Request -> IO (Either ServerError OIDCUser)
authenticateOIDC cfg req = runExceptT $ do
  token  <- maybe (throwError err401') return $ getToken req
  jwt    <- getJWT token
  disco  <- getDisco cfg
  jwkSet <- getJWKSet cfg disco
  claims <- getClaims cfg jwt jwkSet

  let guid     = claims ^? unregisteredClaims . ix "object_guid" . _String
      username = claims ^? unregisteredClaims . ix "preferred_username" . _String

  uid <- maybe
    (die ERROR (throwError err401') ("No object_guid found" :: Text))
    return
    (guid <|> (if oidcAllowServiceToken cfg then username else Nothing))

  let roles = oidcRoles cfg claims

  liftIO $ sequence_ $ catMaybes
    [ userIdVaultKey <?> req <&> flip writeIORef (Just uid)
    , tokenVaultKey  <?> req <&> flip writeIORef (Just $ decodeUtf8 token)
    , pTokenVaultKey <?> req <&> flip writeIORef (Just claims)
    , rolesVaultKey  <?> req <&> flip writeIORef (Just roles)
    ]

  return OIDCUser
    { oidcUserId      = UserId uid
    , oidcAccessToken = decodeUtf8 token
    , oidcParsedToken = claims
    , oidcRoles       = roles
    }
  where
    https mgr = (`httpLbs` mgr)

    err401' :: ServerError
    err401' = err401
      { errBody    = "{\"error\": \"Authorization failed\"}"
      , errHeaders = [(hContentType, "application/json")]
      }

    err500' :: ServerError
    err500' = err500
      { errBody    = "{\"error\": \"Internal server error\"}"
      , errHeaders = [(hContentType, "application/json")]
      }

    die :: (MonadIO m, Show err) => Level -> ExceptT ServerError m b -> err -> ExceptT ServerError m b
    die lvl fin err = liftIO (log' lvl ("web-template" :: Text) $ show err) >> fin

    getToken :: Request -> Maybe ByteString
    getToken r = lookup "Authorization" (requestHeaders r) >>= stripPrefix "Bearer "

    oidcRoles :: OIDCConfig -> ClaimsSet -> [Text]
    oidcRoles OIDCConfig {..} claims = claims
      ^.. unregisteredClaims
      . ix "resource_access"
#if MIN_VERSION_aeson(2, 0, 0)
      . key (fromText oidcClientId)
#else
      . key oidcClientId
#endif
      . key "roles"
      . values . _String

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

    getJWT :: ByteString -> ExceptT ServerError IO SignedJWT
    getJWT = either (die WARNING (throwError err401')) return
      . decodeCompact @_ @JWTError
      . LB.fromStrict

    getDisco :: OIDCConfig -> ExceptT ServerError IO Discovery
    getDisco OIDCConfig {..} = liftIO (Cache.lookup oidcDiscoCache ())
        >>= maybe fetchDisco return
      where
        fetchDisco = liftIO (discovery (https oidcManager) (appWellKnown oidcIssuer))
            >>= either
              (die ERROR (throwError err500'))
              (uncurry discoSuccess)
          where
            discoSuccess disco mbDiscoExp = liftIO $ do
              now <- getCurrentTime
              Cache.insert' oidcDiscoCache (expiration now mbDiscoExp oidcDefaultExpiration) () disco
              return disco

    getJWKSet :: OIDCConfig -> Discovery -> ExceptT ServerError IO JWKSet
    getJWKSet OIDCConfig {..} disco = liftIO (Cache.lookup oidcKeyCache ())
        >>= maybe fetchKeys return
      where
        fetchKeys = liftIO (keysFromDiscovery (https oidcManager) disco)
            >>= either
              (die ERROR (throwError err500'))
              (uncurry keysSuccess)
          where
            keysSuccess jwkSet mbKeysExp = liftIO $ do
              now <- getCurrentTime
              Cache.insert' oidcKeyCache (expiration now mbKeysExp oidcDefaultExpiration) () jwkSet
              return jwkSet

    getClaims :: OIDCConfig -> SignedJWT -> JWKSet -> ExceptT ServerError IO ClaimsSet
    getClaims OIDCConfig {..} jwt jwkSet =
        liftIO (runExceptT $ verifyClaims @_ @_ @JWTError (jwtValidation oidcIssuer oidcClientId) jwkSet jwt)
          >>= either (die ERROR (throwError err401')) return
      where
        jwtValidation :: URI -> Text -> JWTValidationSettings
        jwtValidation issuer audience = defaultJWTValidationSettings (const True)
          & issuerPredicate .~ (\iss -> iss ^? uri == Just issuer)
          & audiencePredicate .~ (\aud -> aud ^? string == Just audience)

instance ( HasServer api context
         , HasContextEntry context OIDCConfig
         ) => HasServer (OIDCAuth :> api) context where

  type ServerT (OIDCAuth :> api) m = OIDCUser -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt . s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck sub
      $ withRequest $ \req ->
          liftIO (authenticateOIDC cfg req) >>= either delayedFailFatal return
    where
      cfg = getContextEntry context

instance HasOpenApi api => HasOpenApi (OIDCAuth :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & components . securitySchemes . securityDefinitions . at "cbdJWT" ?~ idJWT
    & components . securitySchemes . securityDefinitions . at "cbdOIDC" ?~ idOIDC
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
         ) => HasServer (Permit roles :> api) context where

  type ServerT (Permit roles :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck (const <$> sub)
      $ withRequest $ \req -> do
          rolesRef  <- maybe unauth401 return $ rolesVaultKey <?> req
          haveRoles <- liftIO (readIORef rolesRef) >>= maybe unauth401 return
          unless (any (`elem` rolesNeeded) haveRoles)
            unauth403
    where
      rolesNeeded = symbolsVal (Proxy @roles)

instance ( HasOpenApi api
         , KnownSymbols roles
         ) => HasOpenApi (Permit roles :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    -- If there is already 'Permit' on deeper API levels with 403 response,
    -- we should not override it. It may have stricter restrictions, that
    -- need to be represented in Swagger.
    & setResponseWith const 403 (return $ mempty & description .~ descr)
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

swaggerUiIndexBCDTemplate :: Text
#if MIN_VERSION_file_embed_lzma(0,1,0)
swaggerUiIndexBCDTemplate = $$(embedText "index.html.tmpl")
#else
swaggerUiIndexBCDTemplate = $(embedText "index.html.tmpl")
#endif

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
      & components . securitySchemes . securityDefinitions . at "cbdOIDC" ?~
        SecurityScheme
          (SecuritySchemeOpenIdConnect $ URL $ T.pack $ show $ appWellKnown $ oidcIssuer oidcConfig)
          (Just "BIOCAD's OpenID Connect auth")

-- | Append OIDC's @.well-known/openid-configuration" part to the base of OIDC issuer URI.
appWellKnown :: URI -> URI
appWellKnown u@URI {..} = u {uriPath = uriPath <> "/.well-known/openid-configuration"}

securityDefinitions :: Iso' SecurityDefinitions (Definitions SecurityScheme)
securityDefinitions = coerced
