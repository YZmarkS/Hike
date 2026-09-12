{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.User where

import Handlers.Internal
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist.Sql
import Model
import Servant

postUserHandler :: User -> AppM (Entity User)
postUserHandler user = do
  { pool <- asks id
  ; sqlResult <- liftIO $ runSqlPool (insertUniqueEntity user) pool
  ; case sqlResult of
      Nothing -> throwError $ err409 { errBody = "Cannot insert due to uniqueness" }
      Just newUserId -> return newUserId }

getAllUsersHandler :: AppM [Entity User]
getAllUsersHandler = do
  { pool <- asks id
  ; liftIO $ runSqlPool (selectList [] []) pool }
