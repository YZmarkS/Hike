{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Servant.Server.Generic
import Servant.Server.Experimental.Auth
import Model
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status


-- The following packages are useful for dev, but think about them when deploy
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options

type WarpLogFunc = (Request -> Status -> Maybe Integer -> IO ())

monadLoggerToWarpLogger :: LogFunc -> WarpLogFunc
monadLoggerToWarpLogger loggerFunc request status fileSize =
  let logSource = "warp server"
      logStr = "\n\t" <> toLogStr (show request)
        <> "\n\t" <> toLogStr (show status)
        <> "\n\t" <> toLogStr ("File size: " <> show fileSize)
  in loggerFunc defaultLoc logSource LevelDebug logStr

warpSetting :: LogFunc -> Settings
warpSetting logFunc =
  setLogger (monadLoggerToWarpLogger logFunc) $
  setPort 8081 defaultSettings

application :: ConnectionPool -> Application
application pool =
    simpleCors $
    genericServeTWithContext
    (flip runReaderT pool)
    handler
    ((authCheck pool) :. EmptyContext)

handler :: API (AsServerT AppM)
handler =
    API { user = postUserServer
        , trip = tripHandler
        }

tripHandler :: AuthenticatedUser -> TripAPI (AsServerT AppM)
tripHandler authUser =
    TripAPI { tripCollection = tripCollectionHandler authUser
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
