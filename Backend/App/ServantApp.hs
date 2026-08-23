{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantApp where

import API
import API.Handlers.Trip
import API.Handlers.Internal
import Control.Monad
import Database.Persist.Sql
import Servant
import Servant.Server.Generic
import Model



tripMemberResourceHandler :: TripMemberResourceAPI (AsServerT AppM)
tripMemberResourceHandler = TripMemberResourceAPI {}

tripCollectionHandler :: UserId -> TripCollectionAPI (AsServerT AppM)
tripCollectionHandler userId =
    TripCollectionAPI { getTrips = getTripsServer
                      -- , postTrip = postTripServer userId
                      }

tripsHandler :: ConnectionPool -> UserId -> TripsAPI (AsServerT AppM)
tripsHandler pool userId =
    TripsAPI { tripCollection = tripCollectionHandler userId
             , tripOwnerResource = const tripOwnerFailHandler
             -- , tripOwnerResource = \tripId -> do
             --     { maybeTrip <- runSqlPool (get tripId) pool
             --     ; trip <- case maybeTrip of
             --                 Nothing -> throwError err401
             --                 Just trip -> return trip
             --     ; when (userId /= tripOwnerId trip) (throwError err401)
             --     ; return $ tripOwnerResourceHandler tripId }
             -- , tripMemberResource = const tripMemberResourceHandler
             }

tripOwnerFailHandler :: TripOwnerResourceAPI (AsServerT AppM)
tripOwnerFailHandler =
    TripOwnerResourceAPI { deleteTrip = throwError err403 }

tripOwnerResourceHandler :: TripId -> TripOwnerResourceAPI (AsServerT AppM)
tripOwnerResourceHandler tripId =
    TripOwnerResourceAPI { deleteTrip = deleteTripServer tripId }
