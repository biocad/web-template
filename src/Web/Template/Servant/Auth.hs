{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Web.Template.Servant.Auth
  ( CbdAuth
  , OIDCAuth
  , UserId (..)
  , OIDCConfig (..)
  , defaultOIDCCfg
  ) where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import           Control.Applicative    ((<|>))
import           Control.Lens           (At (at), ix, (&), (.~), (<&>), (?~), (^?!))
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef             (writeIORef)
import           Data.Maybe             (catMaybes)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Vault.Lazy        as V
import           GHC.Generics           (Generic)

import           Crypto.JOSE.JWK                (JWKSet)
import           Crypto.JWT                     (JWTError, decodeCompact,
                                                 defaultJWTValidationSettings, unregisteredClaims,
                                                 verifyClaims)
import           Data.Aeson                     (Value (..))
import           Data.ByteString                (stripPrefix)
import qualified Data.ByteString.Lazy           as LB
import           Data.Cache                     (Cache)
import qualified Data.Cache                     as Cache
import           Data.OpenApi.Internal          (ApiKeyLocation (..), ApiKeyParams (..),
                                                 HttpSchemeType (..), SecurityRequirement (..),
                                                 SecurityScheme (..), SecuritySchemeType (..))
import           Data.OpenApi.Lens              (components, description, security, securitySchemes)
import           Data.OpenApi.Operation         (allOperations, setResponse)
import           Data.Time.Clock                (diffUTCTime, getCurrentTime,
                                                 nominalDiffTimeToSeconds)
import           Network.HTTP.Client            (Manager, httpLbs)
import           Network.HTTP.Client.TLS        (newTlsManager)
import           Network.HTTP.Types.Header      (hContentType)
import           Network.URI                    (URI)
import           Network.Wai                    (requestHeaders, vault)
import           OpenID.Connect.Client.Provider (Discovery (Discovery, jwksUri), keysFromDiscovery)
import qualified OpenID.Connect.Client.Provider as OIDC
import           Servant.API                    ((:>))
import           Servant.OpenApi                (HasOpenApi (..))
import           Servant.Server                 (HasContextEntry (getContextEntry), HasServer (..),
                                                 ServerError (..), err401)
import           Servant.Server.Internal        (addAuthCheck, delayedFailFatal, withRequest)
import           System.Clock                   (TimeSpec (sec))
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

-- | Adds authenthication via jwt
--
-- Usage:
--
-- > type API = OIDCAuth :> (....)
-- Tooks token from 'Authorization' header
-- Handlers will get an 'UserId' argument
-- Stores token and claims in vault
data OIDCAuth

-- | Info needed for OIDC authorization & key cache
data OIDCConfig = OIDCConfig
  { oidcManager       :: Manager -- ^ https manager
  , oidcClientId      :: Text -- ^ audience
  , oidcIssuer        :: URI -- ^ discovery uri
  , oidcWorkaroundUri :: URI -- ^ temporary solution to openid-connect issue
  , oidcKeyCache      :: Cache Text JWKSet -- ^ cache - storing validation keys
  }

defaultOIDCCfg :: IO OIDCConfig
defaultOIDCCfg = do
  cache <- Cache.newCache (Just 0)
  mgr <- newTlsManager
  return $ OIDCConfig
    { oidcManager = mgr
    , oidcKeyCache = cache
    , oidcWorkaroundUri = undefined
    , oidcIssuer = undefined
    , oidcClientId = undefined
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

        token <- case getHeader req of
          Just token -> return token
          Nothing    -> delayedFailFatal err

        jws <- case getHeader req >>= getToken of
          Just jws -> return jws
          Nothing  -> delayedFailFatal err

        let OIDCConfig {..} = getContextEntry context

        jwkSet <- liftIO (Cache.lookup oidcKeyCache "jwkSet") >>= \case
          Just jwkSet -> return jwkSet
          Nothing     -> liftIO
              ( keysFromDiscovery
                (https oidcManager)
                (Discovery {jwksUri = OIDC.URI $ oidcWorkaroundUri})
              ) >>= \case
                Left jwtErr -> do
                  logWarn $ show jwtErr
                  delayedFailFatal err
                Right (jwkSet, mbKeysExp) -> liftIO $ do
                  now <- getCurrentTime
                  Cache.insert' oidcKeyCache
                    (diffTime <$> (mbKeysExp <|> pure now) <*> pure now)
                    "jwkSet"
                    jwkSet
                  return jwkSet

        claims <- liftIO
          ( runExceptT $
            verifyClaims @_ @_ @JWTError
              (defaultJWTValidationSettings audCheck)
              jwkSet
              jws
          ) >>= \case
            Left jwtErr  -> do
              logWarn $ show jwtErr
              delayedFailFatal err
            Right claims -> return claims

        let String uid = claims
              ^?! unregisteredClaims
              .ix "object_guid"

        liftIO $ sequence_ $ catMaybes
          [ userIdVaultKey <?> req <&> flip writeIORef (Just uid)
          , tokenVaultKey  <?> req <&> flip writeIORef (Just $ decodeUtf8 token)
          , pTokenVaultKey <?> req <&> flip writeIORef (Just claims)
          ]

        return $ UserId uid
    where
      https mgr = (`httpLbs` mgr)

      getHeader r = lookup "Authorization" (requestHeaders r) >>= stripPrefix "Bearer "

      getToken = either (const Nothing) Just . decodeCompact @_ @JWTError . LB.fromStrict

      logWarn = liftIO . log' WARNING ("web-template" :: Text)

      diffTime from to = let
          tTreshold = 60
          diff = diffUTCTime from to
          mkTS x = 0 {sec = x}
        in max 0 $ mkTS $ floor $ nominalDiffTimeToSeconds $ diff - tTreshold

      audCheck = const True

      what <?> req = V.lookup what $ vault req

      err = err401
        { errBody = "{\"error\": \"Authorization failed\"}"
        , errHeaders = [(hContentType, "application/json")]
        }

instance HasOpenApi api => HasOpenApi (OIDCAuth :> api) where
  toOpenApi _ = toOpenApi @api Proxy
    & components . securitySchemes . at "cbdJWT" ?~ idJWT
    & allOperations . security .~ [SecurityRequirement $ mempty & at "cbdJWT" ?~ []]
    & setResponse 401 (return $ mempty & description .~ "Authorization failed")
    where
      idJWT = SecurityScheme
        (SecuritySchemeHttp $ HttpSchemeBearer $ Just "jwt")
        (Just "jwt token")
