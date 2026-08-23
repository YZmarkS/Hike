{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth where

import Data.Aeson
import Data.Text
import GHC.Generics
import Database.Persist.Sql
import Model
import Servant as S
-- import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Control.Monad.IO.Class (liftIO)

newtype AuthenticatedUser =
    AuthenticatedUser { userId :: UserId
                      } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

authCheck :: ConnectionPool -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
authCheck pool (BasicAuthData username _) =
    do { candidates <- liftIO $ runSqlPool (selectList [UserUsername ==. pack (show username)] []) pool
       ; return $ case candidates of
                    [] -> SAS.Indefinite
                    (user : _) -> Authenticated (AuthenticatedUser { userId = entityKey user }) }

instance FromBasicAuthData AuthenticatedUser where
    fromBasicAuthData authData authCheckFunction = authCheckFunction authData
