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
  , getNothing = getNothingHandler
  , postRefresh = postRefreshHandler
  }

tripHandler :: AccessUserId -> TripAPI (AsServerT AppM)
tripHandler accessUserId = MkTripAPI
  { tripCollection = tripCollectionHandler accessUserId
  , tripResource = tripResourceHandler accessUserId
  , placeResource = placeResourceHandler accessUserId
  , membershipResource = membershipResourceHandler accessUserId
  }

tripCollectionHandler :: AccessUserId -> TripCollectionAPI (AsServerT AppM)
tripCollectionHandler accessUserId = MkTripCollectionAPI
  { postTrip = postTripHandler accessUserId
  , getUserTrips = getUserTripsHandler accessUserId
  }

tripResourceHandler :: AccessUserId -> TripId -> TripResourceAPI (AsServerT AppM)
tripResourceHandler accessUserId tripId = MkTripResourceAPI
  { patchRename = patchRenameHandler accessUserId tripId
  , deleteTrip = deleteTripHandler accessUserId tripId
  }

placeResourceHandler :: AccessUserId -> TripId -> PlaceResourceAPI (AsServerT AppM)
placeResourceHandler accessUserId tripId = MkPlaceResourceAPI
  { postPlace = postPlaceHandler accessUserId tripId
  , getPlaces = getPlacesHandler accessUserId tripId
  }

membershipResourceHandler :: AccessUserId -> TripId -> MembershipResourceAPI (AsServerT AppM)
membershipResourceHandler accessUserId tripId = MkMembershipResourceAPI
  { postNewMembership = postNewMembershipServer accessUserId tripId
  , getTripMembers = getTripMembersServer accessUserId tripId
  }

applicationCreation :: ConnectionPool -> IO Application
applicationCreation pool = do
  { jsonWebKey <- generateJWKForJWT
  ; let jwtConfig = defaultJWTSettings jsonWebKey
        authConfig = authCheck pool
        accessAuthHandler = mkAccessAuthHandler jsonWebKey
        refreshAuthHandler = mkRefreshAuthHandler pool jsonWebKey
        context = accessAuthHandler
          :. refreshAuthHandler
          :. jwtConfig
          :. defaultCookieSettings
          :. authConfig
          :. EmptyContext
  ; return $
    simpleCors $
    provideOptions (genericApi (Proxy @API)) $
    genericServeTWithContext
    (`runReaderT` (MkAppState { dbPool = pool, jwk = jsonWebKey }))
    handler
    context }
