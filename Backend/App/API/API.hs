{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module API where

import GHC.Generics
import Model
import Data.Text
import Database.Persist
import Servant
import Servant.Auth as SA
import Types
import Types.User

data API mode = MkAPI
  { admin :: mode :- "admin" :> NamedRoutes AdminAPI
  , user :: mode :- "user" :> NamedRoutes UserAPI
  , trip :: mode :- Auth '[SA.BasicAuth] UserId :> "trips" :> NamedRoutes API.TripAPI
  } deriving (Generic)

data AdminAPI mode = MkAdminAPI
  { getAllUsers :: mode :- "all_users" :> Get '[JSON] [PublicUserData]
  , getAllTrips :: mode :- "all_trips" :> Get '[JSON] [Entity Trip]
  } deriving (Generic)

data UserAPI mode = MkUserAPI
  { postUser :: mode :- "register" :> ReqBody '[JSON] SignUp :> PostCreated '[JSON] UserId
  , postLogin :: mode :- "login" :> ReqBody '[JSON] Login :> PostCreated '[JSON] UserId
  } deriving (Generic)

data TripAPI mode = MkTripAPI
  { tripCollection ::
      mode :- NamedRoutes TripCollectionAPI
  , tripResource ::
      mode :- Capture "trip_id" TripId :> NamedRoutes TripResourceAPI
  , placeResource ::
      mode :- Capture "trip_id" TripId :> NamedRoutes PlaceResourceAPI
  , membershipResource ::
      mode :- Capture "trip_id" TripId :> NamedRoutes MembershipResourceAPI
  } deriving (Generic)

data TripCollectionAPI mode = MkTripCollectionAPI
  { postTrip ::
      mode :- ReqBody '[JSON] Trip :> PostCreated '[JSON] TripId
  , getUserTrips ::
      mode :- Get '[JSON] [Entity Trip]
  } deriving (Generic)

data TripResourceAPI mode = MkTripResourceAPI
  { patchRename ::
      mode :- "rename" :> ReqBody '[JSON] Text :> Patch '[JSON] String
  , deleteTrip ::
      mode :- Delete '[JSON] String
  } deriving (Generic)

data GoalResourceAPI mode = MkGoalResourceAPI
  { postGoal ::
      mode :- ReqBody '[JSON] Goal :> PostCreated '[JSON] GoalId
  , patchGoal ::
      mode :- ReqBody '[JSON] Goal :> Patch '[JSON] String
  , getGoals ::
      mode :- Get '[JSON] [Entity Goal]
  } deriving (Generic)

data PlaceResourceAPI mode = MkPlaceResourceAPI
  { postPlace ::
      mode :- ReqBody '[JSON] Place :> PostCreated '[JSON] PlaceId
  , getPlaces ::
      mode :- Get '[JSON] [Entity Place]
  } deriving (Generic)

-- data ItineraryResourceAPI mode = MkItineraryAPI
--   { postItinerary ::
--       mode :- ReqBody '[JSON] Itinerary :> PostCreated '[JSON] Itinerary
--   } deriving (Generic)

data MembershipResourceAPI mode = MkMembershipResourceAPI
  { postNewMembership ::
      mode :- ReqBody '[JSON] UserId :> PostCreated '[JSON] MembershipId
  , getTripMembers ::
      mode :- Get '[JSON] [PublicUserData]
  } deriving (Generic)
