{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth where

import API.Handlers.Internal
import Data.Aeson
import Data.Text.Encoding
import GHC.Generics
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

type AuthUserId = AuthResult UserId

instance ToJWT UserId
instance FromJWT UserId

type instance BasicAuthCfg = BasicAuthData -> IO AuthUserId

authCheck :: ConnectionPool -> BasicAuthData -> IO AuthUserId
authCheck pool (BasicAuthData username _) =
    do { candidates <- liftIO $
                       runSqlPool
                       (selectList [UserUsername ==. decodeUtf8Lenient username] [])
                       pool
       ; print candidates
       ; return $ case candidates of
                    [] -> Indefinite
                    (user : _) -> Authenticated (entityKey user) }

instance FromBasicAuthData UserId where
    fromBasicAuthData authData authCheckFunction = authCheckFunction authData

extractUserId :: AuthUserId -> AppM UserId
extractUserId (Authenticated userId) = return userId
extractUserId _ = throwError $ err401 { errBody = "Did not find user with same username" }


instance (HasForeign lang ftype api) =>
  HasForeign lang ftype (Auth k a :> api) where

  type Foreign ftype (Auth k a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR
