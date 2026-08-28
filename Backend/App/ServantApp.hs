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


handler :: API (AsServerT AppM)
handler =
    API { user = postUserServer
        , trip = \authUser -> tripHandler authUser
        }

tripHandler :: AuthenticatedUser -> TripsAPI (AsServerT AppM)
tripHandler authUser =
    TripsAPI { tripCollection = tripCollectionHandler authUser
             , tripOwnerResource = tripOwnerResourceHandler authUser
             }

tripCollectionHandler :: AuthenticatedUser -> TripCollectionAPI (AsServerT AppM)
tripCollectionHandler authUser =
    TripCollectionAPI { postTrip = postTripServer authUser
                      , getUserTrips = getUserTripsServer authUser
                      , getAllTrips = getAllTripsServer
                      }

tripOwnerResourceHandler :: AuthenticatedUser -> TripId -> TripOwnerResourceAPI (AsServerT AppM)
tripOwnerResourceHandler authUser tripId =
    TripOwnerResourceAPI { deleteTrip = deleteTripServer authUser tripId }
