{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Endpoint.Place
  ( PlaceAPI
  , postPlaceServer
  , getPlacesServer
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Database.Persist.Sql
import Servant
import Model

type PostPlace = Capture "trip_id" TripId :> ReqBody '[JSON] Place :> PostCreated '[JSON] PlaceId

postPlaceServer :: ConnectionPool -> Server PostPlace
postPlaceServer pool pathTripId place = do
  let bodyTripId = placeTripId place
  when (pathTripId /= bodyTripId) (throwError $ err400 { errBody = "Inconsistent trip id" })
  sqlResult <- liftIO $ runSqlPool (insertBy place) pool
  case sqlResult of
    Left _ -> throwError $ err409 { errBody = "Place already exists" }
    Right newId -> return newId

type GetPlaces = Capture "trip_id" TripId :> "places" :> Get '[JSON] [Entity Place]

getPlacesServer :: ConnectionPool -> Server GetPlaces
getPlacesServer pool tripId = do
  liftIO $ runSqlPool (selectList [PlaceTripId ==. tripId] []) pool

type PlaceAPI = PostPlace :<|> GetPlaces
