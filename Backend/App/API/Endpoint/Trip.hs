{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Endpoint.Trip
  ( TripAPI
  , postTripServer
  , getTripsServer
  , deleteTripServer
  ) where

import API.Endpoint.Internal
import Data.String
import Data.ByteString.Lazy
import Control.Monad.Reader
import Control.Monad.IO.Class
import Database.Persist.Sql
import Servant
import Model

type PostTrip = "trip" :> ReqBody '[JSON] Trip :> PostCreated '[JSON] (Key Trip)

postTripServer :: Trip -> AppM (Key Trip)
postTripServer trip = do
  pool <- asks id
  sqlResult <- liftIO $ runSqlPool (insertBy trip) pool
  case sqlResult of
    Left trip' -> let errBody = if tripName trip == tripName (entityVal trip')
                                then "Trip with same name already exists"
                                else append
                                     "Cannot insert due to trip id: "
                                     (fromString $ show $ entityKey trip' ) -- is there no better way?
                  in throwError $ err409 { errBody }
    Right newId -> return newId

type GetTrips = "trip" :> Get '[JSON] [Entity Trip]

getTripsServer :: AppM [Entity Trip]
getTripsServer = do
  pool <- asks id
  liftIO $ runSqlPool (selectList [] []) pool

type DeleteTrip = "trip" :> QueryParam' '[Required, Strict] "id" TripId :> Delete '[JSON] String

deleteTripServer :: TripId -> AppM String
deleteTripServer tripId = do
  pool <- asks id
  liftIO $ runSqlPool (deleteWhere [TripId ==. tripId]) pool
  return "Deleted"

type TripAPI = PostTrip :<|> GetTrips :<|> DeleteTrip
