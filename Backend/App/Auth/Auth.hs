{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Auth where

import Data.Text.Encoding
import Database.Persist.Sql
import Model
import Servant.Foreign
import Servant
import Servant.Auth
import Servant.Auth.Server
import Control.Monad.IO.Class (liftIO)

-- newtype AuthenticatedUser =
--     AuthenticatedUser { userId :: UserId
--                       } deriving (Show, Generic)

-- instance ToJSON AuthenticatedUser
-- instance FromJSON AuthenticatedUser

-- instance FromBasicAuthData AuthenticatedUser where
--     fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type HikeAuthResult = AuthResult UserId

instance ToJWT UserId
instance FromJWT UserId

type instance BasicAuthCfg = BasicAuthData -> IO HikeAuthResult

authCheck :: ConnectionPool -> BasicAuthData -> IO HikeAuthResult
authCheck pool (BasicAuthData username _) =
    do { candidates <- liftIO $
                       runSqlPool
                       (selectList [UserUsername ==. decodeUtf8Lenient username] [])
                       pool
       ; return $ case candidates of
                    [] -> Indefinite
                    (user : _) -> Authenticated (entityKey user) }

instance FromBasicAuthData UserId where
    fromBasicAuthData authData authCheckFunction = authCheckFunction authData

-- Known integration issue between servant-options and authentication:
-- https://github.com/sordina/servant-options/issues/2
instance (HasForeign lang ftype api) =>
  HasForeign lang ftype (Auth k a :> api) where

  type Foreign ftype (Auth k a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR
