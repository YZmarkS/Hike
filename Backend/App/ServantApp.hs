{-# LANGUAGE DataKinds #-}

module ServantApp where

import API
import API.Handlers
import Auth
import Control.Monad.Reader
import Database.Persist.Sql
import Servant
import Servant.Auth.Server
import Servant.Server.Generic
import Model

-- The following packages are useful for dev, but think about them when deploy
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options


handler :: API (AsServerT AppM)
handler =
    API { user = userHandler
        , trip = tripHandler
        }

userHandler :: UserAPI  (AsServerT AppM)
userHandler =
    UserAPI { postUser = postUserServer
            , getAllUsers = getAllUsersServer
            }

tripHandler :: HikeAuthResult -> TripAPI (AsServerT AppM)
tripHandler hikeAuthResult =
    TripAPI { tripCollection = tripCollectionHandler hikeAuthResult
             , tripOwnerResource = tripOwnerResourceHandler hikeAuthResult
             }

tripCollectionHandler :: HikeAuthResult -> TripCollectionAPI (AsServerT AppM)
tripCollectionHandler hikeAuthResult =
    TripCollectionAPI { postTrip = postTripServer hikeAuthResult
                      , getUserTrips = getUserTripsServer hikeAuthResult
                      , getAllTrips = getAllTripsServer
                      }

tripOwnerResourceHandler :: HikeAuthResult -> TripId -> TripOwnerResourceAPI (AsServerT AppM)
tripOwnerResourceHandler hikeAuthResult tripId =
    TripOwnerResourceAPI { deleteTrip = deleteTripServer hikeAuthResult tripId }

applicationCreation :: ConnectionPool -> IO Application
applicationCreation pool = do
  { jwtSigningKey <- generateKey
  ; let jwtConfig = defaultJWTSettings jwtSigningKey
        authConfig = authCheck pool
        context = jwtConfig :. defaultCookieSettings :. authConfig :. EmptyContext
  ; return $
    simpleCors $
    provideOptions (genericApi (Proxy @API)) $
    genericServeTWithContext
    (flip runReaderT pool)
    handler
    context }
