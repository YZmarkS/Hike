{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module API where

import GHC.Generics
import API.Handlers
import Auth
import Model
import Database.Persist
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS

data API mode = API
    { user :: mode :- "user" :> UserAPI
    , trip :: mode :- Auth '[SA.BasicAuth] AuthenticatedUser :> "trip" :> NamedRoutes TripsAPI
    } deriving (Generic)

data TripsAPI mode = TripsAPI
    { tripCollection :: mode :- "trips" :> NamedRoutes TripCollectionAPI
    , tripOwnerResource :: mode :- "trips" :> Capture "trip_id" TripId :> NamedRoutes TripOwnerResourceAPI
    -- , tripMemberResource :: mode :- "trips" :> Capture "trip_id" TripId :> NamedRoutes TripMemberResourceAPI
    } deriving (Generic)

data TripOwnerResourceAPI mode = TripOwnerResourceAPI
    { deleteTrip :: mode :- Delete '[JSON] String
    } deriving (Generic)

data TripCollectionAPI mode = TripCollectionAPI
    { getTrips :: mode :- Get '[JSON] [Entity Trip]
    -- , postTrip :: mode :- ReqBody '[JSON] Trip :> PostCreated '[JSON] TripId
    } deriving (Generic)

data TripMemberResourceAPI mode = TripMemberResourceAPI
    { } deriving (Generic)
