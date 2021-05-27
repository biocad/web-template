{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Template.Servant.Auth
  ( CbdAuth
  , OIDCAuth
  , UserId (..)
  , OIDCConfig (..)
  , defaultOIDCCfg
  , Permit
  ) where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import           Control.Applicative    ((<|>))
import           Control.Lens           (At (at), ix, (&), (.~), (<&>), (?~), (^?), (^..))
import           Control.Monad.Except   (runExceptT, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef             (writeIORef, readIORef)
import           Data.Maybe             (catMaybes)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text, pack, intercalate)
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Vault.Lazy        as V
import           GHC.Generics           (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import           Crypto.JOSE.JWK                (JWKSet)
import           Crypto.JWT                     (JWTError, JWTValidationSettings, audiencePredicate,
                                                 decodeCompact, defaultJWTValidationSettings,
                                                 issuerPredicate, string, unregisteredClaims, uri,
                                                 verifyClaims)
import           Data.Aeson.Lens                (AsPrimitive (_String), key, values)
import           Data.ByteString                (stripPrefix)
import qualified Data.ByteString.Lazy           as LB
import           Data.Cache                     (Cache)
import qualified Data.Cache                     as Cache
import           Data.OpenApi.Internal          (ApiKeyLocation (..), ApiKeyParams (..),
                                                 HttpSchemeType (..), SecurityRequirement (..),
                                                 SecurityScheme (..), SecuritySchemeType (..))
import           Data.OpenApi.Lens              (components, description, security, securitySchemes)
import           Data.OpenApi.Operation         (allOperations, setResponse)
import           Data.Time.Clock                (UTCTime, diffUTCTime, getCurrentTime,
                                                 nominalDiffTimeToSeconds)
import           Network.HTTP.Client            (Manager, httpLbs)
import           Network.HTTP.Client.TLS        (newTlsManager)
import           Network.HTTP.Types.Header      (hContentType)
import           Network.URI                    (URI (..))
import           Network.Wai                    (Request, requestHeaders, vault)
import           OpenID.Connect.Client.Provider (Discovery, discovery, keysFromDiscovery)
import           Servant.API                    ((:>))
import           Servant.OpenApi                (HasOpenApi (..))
import           Servant.Server                 (HasContextEntry (getContextEntry), HasServer (..),
                                                 ServerError (..), err401, err403)
import           Servant.Server.Internal        (DelayedIO, addAuthCheck, delayedFailFatal,
                                                 withRequest)
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
-- Takes token from 'Authorization' header
-- Handlers will get an 'UserId' argument
-- Stores token and claims in vault
data OIDCAuth

-- | Info needed for OIDC authorization & key cache
data OIDCConfig = OIDCConfig
  { oidcManager       :: Manager -- ^ https manager
  , oidcClientId      :: Text -- ^ audience
  , oidcIssuer        :: URI -- ^ discovery uri
  , oidcDiscoCache    :: Cache () Discovery -- ^ cache - storing discovery information
  , oidcKeyCache      :: Cache () JWKSet -- ^ cache - storing validation keys
  }

defaultOIDCCfg :: IO OIDCConfig
defaultOIDCCfg = do
  discoCache <- Cache.newCache $ Just 0
  keyCache <- Cache.newCache $ Just 0
  mgr <- newTlsManager
  return $ OIDCConfig
    { oidcManager = mgr
    , oidcDiscoCache = discoCache
    , oidcKeyCache = keyCache
    , oidcIssuer = error "discovery uri not set"
    , oidcClientId = error "client id not set"
    }

instance ( HasServer api context
         , HasContextEntry context OIDCConfig
         ) => HasServer (OIDCAuth :> api) context where

  type ServerT (OIDCAuth :> api) m = UserId -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext @api Proxy pc nt . s

  route _ context sub =
    route @api Proxy context
      $ addAuthCheck sub
      $ withRequest $ \req -> do

        token <- maybe unauth401 return (getToken req)

        jws <- case decodeToken token of
          Left jwtErr -> do
            logWarn $ show jwtErr
            unauth401
          Right jws -> return jws

        let OIDCConfig {..} = getContextEntry context

        disco <- liftIO (Cache.lookup oidcDiscoCache ()) >>= \case
          Nothing -> liftIO
            ( discovery
              (https oidcManager)
              (appWellKnown oidcIssuer)
            ) >>= \case
              Left discoErr -> do
                logWarn $ show discoErr
                unauth401
              Right (disco, mbDiscoExp) -> liftIO $ do
                now <- getCurrentTime
                Cache.insert' oidcDiscoCache
                    (diffTime <$> (mbDiscoExp <|> pure now) <*> pure now)
                    ()
                    disco
                return disco
          Just disco -> return disco

        jwkSet <- liftIO (Cache.lookup oidcKeyCache ()) >>= \case
          Nothing -> liftIO
              ( keysFromDiscovery
                (https oidcManager)
                disco
              ) >>= \case
                Left jwtErr -> do
                  logWarn $ show jwtErr
                  unauth401
                Right (jwkSet, mbKeysExp) -> liftIO $ do
                  now <- getCurrentTime
                  Cache.insert' oidcKeyCache
                    (diffTime <$> (mbKeysExp <|> pure now) <*> pure now)
                    ()
                    jwkSet
                  return jwkSet
          Just jwkSet -> return jwkSet

        claims <- liftIO
          ( runExceptT $
            verifyClaims @_ @_ @JWTError
              (jwtValidation oidcIssuer oidcClientId)
              jwkSet
              jws
          ) >>= \case
            Left jwtErr -> do
              logWarn $ show jwtErr
              unauth401
            Right claims -> return claims

        uid <- case claims ^? unregisteredClaims . ix "object_guid" . _String of
          Nothing -> do
            logErr ("No object_guid found" :: Text)
            unauth401
          Just uid -> return uid

        liftIO $ sequence_ $ catMaybes
          [ userIdVaultKey <?> req <&> flip writeIORef (Just uid)
          , tokenVaultKey  <?> req <&> flip writeIORef (Just $ decodeUtf8 token)
          , pTokenVaultKey <?> req <&> flip writeIORef (Just claims)
          ]

        return $ UserId uid
    where
      https mgr = (`httpLbs` mgr)

      getToken r = lookup "Authorization" (requestHeaders r) >>= stripPrefix "Bearer "

      decodeToken = decodeCompact @_ @JWTError . LB.fromStrict

      appWellKnown u@URI {..} = u {uriPath = uriPath <> "/.well-known/openid-configuration"}

      logWarn = liftIO . log' WARNING ("web-template" :: Text)

      logErr = liftIO . log' ERROR ("web-template" :: Text)

      diffTime :: UTCTime -> UTCTime -> TimeSpec
      diffTime from to = let
          diff = diffUTCTime from to - tTreshold
        in max
            TimeSpec {sec = 0, nsec = 0}
            TimeSpec {sec = floor $ nominalDiffTimeToSeconds diff, nsec = 0}
        where
          tTreshold = 60 -- consider token expired 'tTreshold' seconds earlier

      jwtValidation :: URI -> Text -> JWTValidationSettings
      jwtValidation issuer audience = defaultJWTValidationSettings (const True)
        & issuerPredicate .~ (\iss -> iss ^? uri == Just issuer)
        & audiencePredicate .~ (\aud -> aud ^? string == Just audience)

instance HasOpenApi api => HasOpenApi (OIDCAuth :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & components . securitySchemes . at "cbdJWT" ?~ idJWT
    & allOperations . security .~ [SecurityRequirement $ mempty & at "cbdJWT" ?~ []]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idJWT = SecurityScheme
        (SecuritySchemeHttp $ HttpSchemeBearer $ Just "jwt")
        (Just "jwt token")

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
