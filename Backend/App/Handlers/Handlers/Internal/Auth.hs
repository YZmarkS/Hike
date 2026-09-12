{-# LANGUAGE OverloadedStrings #-}

module Handlers.Internal.Auth where

import Auth
import Control.Monad
import Control.Monad.Reader
import Servant
import Servant.Auth.Server
import Model
import Handlers.Internal
import Database.Persist.Sql

extractUserId :: HikeAuthResult -> AppM UserId
extractUserId (Authenticated userId) = return userId
extractUserId _ = throwError $ err401 { errBody = "Failed to Authenticate" }

isMemberOf :: UserId -> TripId -> AppM ()
isMemberOf userId tripId = do
  { pool <- asks id
  ; maybeMembership <- liftIO $ runSqlPool
                       (getBy (UniqueUserInTrip userId tripId))
                       pool
  ; case maybeMembership of
      Nothing -> throwError $ err403 { errBody = "Not member" }
      Just _ -> return () }

isOwnerOf :: UserId -> TripId -> AppM ()
isOwnerOf userId tripId = do
  { pool <- asks id
  ; tripResult <- liftIO $ runSqlPool (get tripId) pool
  ; isNotOwner <- case tripResult of
                 Nothing -> throwError $ err403 { errBody = "No Trip" }
                 Just trip -> return $ tripOwnerId trip /= userId
  ; when isNotOwner (throwError $ err403 { errBody = "Not owner" }) }
