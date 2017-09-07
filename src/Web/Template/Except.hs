{-# LANGUAGE DeriveGeneric     #-}
module Web.Template.Except
  ( Except (..), JsonWebError (..)
  , handleEx
  ) where

import           Data.Aeson                (ToJSON (..))
import           Data.String               (fromString)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status (status403, status404,
                                            status500)
import           Web.Scotty.Trans          (ActionT, ScottyError (..), json,
                                            status)


instance ScottyError Except where
    stringError = StringEx
    showError = fromString . show

data Except = Forbidden | NotFound Int | StringEx String
    deriving (Show, Eq)

newtype JsonWebError = JsonWebError { error :: String }
  deriving (Generic)

instance ToJSON JsonWebError

handleEx :: Monad m => Except -> ActionT Except m ()
handleEx Forbidden = do
    status status403
    json . JsonWebError $ "Forbidden from server"
handleEx (NotFound i) = do
    status status404
    json . JsonWebError $ "Can't find " ++ show i
handleEx (StringEx str)  = do
    status status500
    json . JsonWebError $ "Server problem: " ++ str
