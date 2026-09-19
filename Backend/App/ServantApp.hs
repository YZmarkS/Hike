{-# LANGUAGE DataKinds #-}

module ServantApp where

import API
import Handlers
import Auth
import Control.Monad.Reader
import Database.Persist.Sql
import Servant
import Servant.Auth.Server
import Servant.Server.Generic
import Model
import Types

-- The following packages are useful for dev, but think about them when deploy
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options

handler :: API (AsServerT AppM)
handler = MkAPI
  { admin = adminHandler
  , user = userHandler
  , trip = tripHandler
  }

adminHandler :: AdminAPI (AsServerT AppM)
adminHandler = MkAdminAPI
  { getAllUsers = getAllUsersHandler
  , getAllTrips = getAllTripsHandler
  }

userHandler :: UserAPI (AsServerT AppM)
userHandler = MkUserAPI
  { postUser = postUserHandler
  , postLogin = postLoginHandler
  }

tripHandler :: HikeAuthResult -> TripAPI (AsServerT AppM)
tripHandler hikeAuthResult = MkTripAPI
  { tripCollection = tripCollectionHandler hikeAuthResult
  , tripResource = tripResourceHandler hikeAuthResult
  , placeResource = placeResourceHandler hikeAuthResult
  , membershipResource = membershipResourceHandler hikeAuthResult
  }

tripCollectionHandler :: HikeAuthResult -> TripCollectionAPI (AsServerT AppM)
tripCollectionHandler hikeAuthResult = MkTripCollectionAPI
  { postTrip = postTripHandler hikeAuthResult
  , getUserTrips = getUserTripsHandler hikeAuthResult
  }

tripResourceHandler :: HikeAuthResult -> TripId -> TripResourceAPI (AsServerT AppM)
tripResourceHandler hikeAuthResult tripId = MkTripResourceAPI
  { patchRename = patchRenameHandler hikeAuthResult tripId
  , deleteTrip = deleteTripHandler hikeAuthResult tripId
  }

placeResourceHandler :: HikeAuthResult -> TripId -> PlaceResourceAPI (AsServerT AppM)
placeResourceHandler hikeAuthResult tripId = MkPlaceResourceAPI
  { postPlace = postPlaceHandler hikeAuthResult tripId
  , getPlaces = getPlacesHandler hikeAuthResult tripId
  }

membershipResourceHandler :: HikeAuthResult -> TripId -> MembershipResourceAPI (AsServerT AppM)
membershipResourceHandler hikeAuthResult tripId = MkMembershipResourceAPI
  { postNewMembership = postNewMembershipServer hikeAuthResult tripId
  , getTripMembers = getTripMembersServer hikeAuthResult tripId
  }

applicationCreation :: ConnectionPool -> IO Application
applicationCreation pool = do
  { jwtSigningKey <- generateJWKForJWT
  ; let jwtConfig = defaultJWTSettings jwtSigningKey
        authConfig = authCheck pool
        context = jwtConfig :. defaultCookieSettings :. authConfig :. EmptyContext
  ; return $
    simpleCors $
    provideOptions (genericApi (Proxy @API)) $
    genericServeTWithContext
    (`runReaderT` (MkAppState { dbPool = pool, jwk = jwtSigningKey }))
    handler
    context }
