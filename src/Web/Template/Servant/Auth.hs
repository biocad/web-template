{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Template.Servant.Auth
  ( CbdAuth
  , OIDCAuth
  , UserId (..)
  ) where

-- after https://www.stackage.org/haddock/lts-15.15/servant-server-0.16.2/src/Servant.Server.Experimental.Auth.html

import           Control.Applicative    ((<|>))
import           Control.Lens           (At (at), _Just, (&), (.~), (<&>), (?~), (^?!))
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef             (writeIORef)
import           Data.Maybe             (catMaybes)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Vault.Lazy        as V
import           GHC.Generics           (Generic)

import           Crypto.JWT                     (JWTError, decodeCompact,
                                                 defaultJWTValidationSettings, unregisteredClaims,
                                                 verifyClaims)
import           Data.Aeson                     (Value (..))
import           Data.ByteString                (stripPrefix)
import qualified Data.ByteString.Lazy           as LB
import qualified Data.Cache                     as Cache
import           Data.OpenApi.Internal          (ApiKeyLocation (..), ApiKeyParams (..),
                                                 HttpSchemeType (..), SecurityRequirement (..),
                                                 SecurityScheme (..), SecuritySchemeType (..))
import           Data.OpenApi.Lens              (components, description, security, securitySchemes)
import           Data.OpenApi.Operation         (allOperations, setResponse)
import           Data.Time.Clock                (diffUTCTime, getCurrentTime,
                                                 nominalDiffTimeToSeconds)
import           Network.HTTP.Client            (httpLbs)
import           Network.HTTP.Types.Header      (hContentType)
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

import Web.Template.Log   (pTokenVaultKey, tokenVaultKey, userIdVaultKey)
import Web.Template.Types (OIDCNeeded (..))

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
         , HasContextEntry context OIDCNeeded
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

        let OIDCNeeded {..} = getContextEntry @_ @OIDCNeeded context

        jwkSet <- liftIO (Cache.lookup oidcKeyCache "jwkSet") >>= \case
          Just jwkSet -> return jwkSet
          Nothing     -> liftIO
              ( keysFromDiscovery
                (https oidcManager)
                (Discovery {jwksUri = OIDC.URI $ oidcDiscoveryUri})
              ) >>= \case
                Left _ -> delayedFailFatal err
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
            Left _error  -> delayedFailFatal err
            Right claims -> return claims

        let String uid = claims
              ^?! unregisteredClaims
              .at "object_guid"
              ._Just

        let mUserIdRef = V.lookup userIdVaultKey $ vault req
        let mTokenRef = V.lookup tokenVaultKey $ vault req
        let mPTokenRef = V.lookup pTokenVaultKey $ vault req

        sequence_ $ liftIO <$> catMaybes
          [ mUserIdRef <&> flip writeIORef (Just uid)
          , mTokenRef  <&> flip writeIORef (Just $ decodeUtf8 token)
          , mPTokenRef <&> flip writeIORef (Just claims)
          ]

        return $ UserId uid
    where
      https mgr = (`httpLbs` mgr)

      getHeader r = lookup "Authorization" (requestHeaders r) >>= stripPrefix "Bearer "

      getToken = either (const Nothing) Just . decodeCompact @_ @JWTError . LB.fromStrict

      tTreshold = 0 {sec = 30}

      diffTime from to = let
          diff = 0 {sec = floor $ nominalDiffTimeToSeconds $ diffUTCTime from to}
        in max 0 $ diff - tTreshold

      audCheck = const True

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
