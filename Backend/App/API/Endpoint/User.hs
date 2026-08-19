{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Endpoint.User
  ( UserAPI
  , postUserServer
  ) where

import Control.Monad.IO.Class
import Database.Persist.Sql
import Model
import Servant

type PostUser = "user" :> ReqBody '[JSON] User :> PostCreated '[JSON] (Entity User)

postUserServer :: ConnectionPool -> Server PostUser
postUserServer pool user = do
  sqlResult <- liftIO $ runSqlPool (insertUniqueEntity user) pool
  case sqlResult of
    Nothing -> throwError $ err409 { errBody = "Cannot insert due to uniqueness" }
    Just newUserId -> return newUserId

type UserAPI = PostUser
