{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.User
  ( UserAPI
  , postUserServer
  ) where

import API.Handlers.Internal
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist.Sql
import Model
import Servant

type PostUser = "user" :> ReqBody '[JSON] User :> PostCreated '[JSON] (Entity User)

postUserServer :: User -> AppM (Entity User)
postUserServer user = do
  pool <- asks id
  sqlResult <- liftIO $ runSqlPool (insertUniqueEntity user) pool
  case sqlResult of
    Nothing -> throwError $ err409 { errBody = "Cannot insert due to uniqueness" }
    Just newUserId -> return newUserId

type UserAPI = PostUser
