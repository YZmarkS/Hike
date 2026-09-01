{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module API where

import GHC.Generics
import Model
import Database.Persist
import Servant
import Servant.Auth as SA

data API mode = API
    { user :: mode :- "user" :> NamedRoutes UserAPI
    , trip :: mode :- Auth '[SA.BasicAuth] UserId :> NamedRoutes API.TripAPI
    } deriving (Generic)

data UserAPI mode = UserAPI
    { postUser :: mode :- ReqBody '[JSON] User :> PostCreated '[JSON] (Entity User)
    , getAllUsers :: mode :- Get '[JSON] [Entity User]
    } deriving Generic

data TripAPI mode = TripAPI
    { tripCollection :: mode :- "trips" :> NamedRoutes TripCollectionAPI
    , tripOwnerResource :: mode :- "trip" :> Capture "trip_id" TripId :> NamedRoutes TripOwnerResourceAPI
    -- , tripMemberResource :: mode :- "trip" :> Capture "trip_id" TripId :> NamedRoutes TripMemberResourceAPI
    } deriving (Generic)

data TripCollectionAPI mode = TripCollectionAPI
    { postTrip :: mode :- ReqBody '[JSON] Trip :> PostCreated '[JSON] TripId
    , getUserTrips :: mode :- Get '[JSON] [Entity Trip]
    , getAllTrips :: mode :- "all" :> Get '[JSON] [Entity Trip]
    } deriving (Generic)

data TripOwnerResourceAPI mode = TripOwnerResourceAPI
    { deleteTrip :: mode :- Delete '[JSON] String
    } deriving (Generic)


-- data TripMemberResourceAPI mode = TripMemberResourceAPI
--     { allMembers :: mode :- "all_members" :> Get '[JSON] [Entity User]
--     } deriving (Generic)
