{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Place where

import Auth
import Handlers.Internal
import Control.Monad.Reader
import Database.Persist.Sql
import Servant
import Model
import Types

postPlaceHandler :: AccessUserId -> TripId -> Place -> AppM PlaceId
postPlaceHandler (AccessUserId userId) tripId place = do
  { userId `isMemberOf` tripId
  ; let canonicalPlace = place { placeTripId = tripId, placeCreatorId = userId }
  ; pool <- asks dbPool
  ; maybePlaceId <- liftIO $ runSqlPool (insertUnique canonicalPlace) pool
  ; case maybePlaceId of
      Nothing -> throwError $ err409 { errBody = "Failure: Place insertion" }
      Just newId -> return newId }

getPlacesHandler :: AccessUserId -> TripId -> AppM [Entity Place]
getPlacesHandler (AccessUserId userId) tripId = do
  { userId `isMemberOf` tripId
  ; pool <- asks dbPool
  ; liftIO $ runSqlPool (selectList [PlaceTripId ==. tripId] []) pool }
