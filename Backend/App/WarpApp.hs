{-# LANGUAGE OverloadedStrings #-}
module WarpApp
  ( warpWebServer
  ) where

import           Database.Persist.Sqlite
import           Control.Monad.Logger

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import API.Endpoint.Place
import API.Endpoint.Trip
import Servant

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

type API = PlaceAPI :<|> TripAPI

warpApplication :: ConnectionPool -> Application
warpApplication pool =
  serve
  (Proxy :: Proxy API)
  (postPlaceServer pool :<|> postTripServer pool :<|> getTripsServer pool)

warpWebServer :: ConnectionPool -> LoggingT IO ()
warpWebServer pool = LoggingT $
  \logFunc -> runSettings (warpSetting logFunc) $ warpApplication pool
