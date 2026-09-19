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

postPlaceHandler :: HikeAuthResult -> TripId -> Place -> AppM PlaceId
postPlaceHandler hikeAuthResult tripId place = do
  { userId <- extractUserId hikeAuthResult
  ; userId `isMemberOf` tripId
  ; let canonicalPlace = place { placeTripId = tripId, placeCreatorId = userId }
  ; pool <- asks dbPool
  ; maybePlaceId <- liftIO $ runSqlPool (insertUnique canonicalPlace) pool
  ; case maybePlaceId of
      Nothing -> throwError $ err409 { errBody = "Cannot insert new place" }
      Just newId -> return newId }

getPlacesHandler :: HikeAuthResult -> TripId -> AppM [Entity Place]
getPlacesHandler hikeAuthResult tripId = do
  { userId <- extractUserId hikeAuthResult
  ; userId `isMemberOf` tripId
  ; pool <- asks dbPool
  ; liftIO $ runSqlPool (selectList [PlaceTripId ==. tripId] []) pool }
