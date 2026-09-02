{-# LANGUAGE DataKinds #-}

module ServantApp where

import API
import API.Handlers
import API.Handlers.Internal
import Auth
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql
import Servant
import Servant.API
import Servant.API.NamedRoutes
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

tripHandler :: AuthUserId -> TripAPI (AsServerT AppM)
tripHandler resultUser =
    TripAPI { tripCollection = tripCollectionHandler resultUser
             , tripOwnerResource = tripOwnerResourceHandler resultUser
             }

tripCollectionHandler :: AuthUserId -> TripCollectionAPI (AsServerT AppM)
tripCollectionHandler resultUser =
    TripCollectionAPI { postTrip = postTripServer resultUser
                      , getUserTrips = getUserTripsServer resultUser
                      , getAllTrips = getAllTripsServer
                      }

tripOwnerResourceHandler :: AuthUserId -> TripId -> TripOwnerResourceAPI (AsServerT AppM)
tripOwnerResourceHandler resultUser tripId =
    TripOwnerResourceAPI { deleteTrip = deleteTripServer resultUser tripId }

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
