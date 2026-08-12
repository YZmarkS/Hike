{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Endpoint.Trip
  ( TripAPI
  , postTripServer
  , getTripsServer
  ) where

import Data.String
import Data.ByteString.Lazy
import Control.Monad.IO.Class
import Database.Persist.Sql
import Servant
import Model

type PostTrip = "trip" :> ReqBody '[JSON] Trip :> PostCreated '[JSON] (Key Trip)

postTripServer :: ConnectionPool -> Trip -> Handler (Key Trip)
postTripServer pool trip = do
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

getTripsServer :: ConnectionPool -> Handler [Entity Trip]
getTripsServer pool = do
  liftIO $ runSqlPool (selectList [] []) pool

type TripAPI = PostTrip :<|> GetTrips
