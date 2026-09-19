{-# LANGUAGE OverloadedStrings #-}

module Handlers.Internal.Permission where

import Auth
import Control.Monad
import Control.Monad.Reader
import Servant
import Servant.Auth.Server
import Model
import Database.Persist.Sql
import Types

extractUserId :: HikeAuthResult -> AppM UserId
extractUserId (Authenticated userId) = return userId
extractUserId _ = throwError $ err401 { errBody = "Failed to Authenticate" }

isMemberOf :: UserId -> TripId -> AppM ()
isMemberOf userId tripId = do
  { pool <- asks dbPool
  ; maybeMembership <- liftIO $ runSqlPool
                       (getBy (UniqueUserInTrip userId tripId))
                       pool
  ; case maybeMembership of
      Nothing -> throwError $ err403 { errBody = "Not member" }
      Just _ -> return () }

isOwnerOf :: UserId -> TripId -> AppM ()
isOwnerOf userId tripId = do
  { pool <- asks dbPool
  ; tripResult <- liftIO $ runSqlPool (get tripId) pool
  ; isNotOwner <- case tripResult of
                 Nothing -> throwError $ err403 { errBody = "No Trip" }
                 Just trip -> return $ tripOwnerId trip /= userId
  ; when isNotOwner (throwError $ err403 { errBody = "Not owner" }) }
