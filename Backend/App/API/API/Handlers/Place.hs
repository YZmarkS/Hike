{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Place where

import Auth
import API.Handlers.Internal
import API.Handlers.Internal.Auth
import Control.Monad.Reader
import Database.Persist.Sql
import Servant
import Model

postPlaceHandler :: HikeAuthResult -> TripId -> Place -> AppM PlaceId
postPlaceHandler hikeAuthResult tripId place = do
  { userId <- extractUserId hikeAuthResult
  ; userId `isMemberOf` tripId
  ; let canonicalPlace = place { placeTripId = tripId, placeCreatorId = userId }
  ; pool <- asks id
  ; maybePlaceId <- liftIO $ runSqlPool (insertUnique canonicalPlace) pool
  ; case maybePlaceId of
      Nothing -> throwError $ err409 { errBody = "Cannot insert new place" }
      Just newId -> return newId }

getPlacesHandler :: HikeAuthResult -> TripId -> AppM [Entity Place]
getPlacesHandler hikeAuthResult tripId = do
  { userId <- extractUserId hikeAuthResult
  ; userId `isMemberOf` tripId
  ; pool <- asks id
  ; liftIO $ runSqlPool (selectList [PlaceTripId ==. tripId] []) pool }
