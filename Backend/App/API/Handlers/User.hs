{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.User where

import API.Handlers.Internal
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist.Sql
import Model
import Servant

postUserServer :: User -> AppM (Entity User)
postUserServer user = do
  { pool <- asks id
  ; sqlResult <- liftIO $ runSqlPool (insertUniqueEntity user) pool
  ; case sqlResult of
      Nothing -> throwError $ err409 { errBody = "Cannot insert due to uniqueness" }
      Just newUserId -> return newUserId }

getAllUsersServer :: AppM [Entity User]
getAllUsersServer = do
  { pool <- asks id
  ; liftIO $ runSqlPool (selectList [] []) pool }
