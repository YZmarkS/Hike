{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WarpApp where

-- import Control.Monad.Logger
-- import Control.Monad.Reader
-- import Network.HTTP.Types.Status
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import API
-- import API.Handlers.Trip
-- import API.Handlers.User
-- import API.Handlers.Place
-- import API.Handlers.Membership
-- import Servant

-- The following packages are useful for dev, but think about them when deploy
-- import Network.Wai.Middleware.Cors
-- import Network.Wai.Middleware.Servant.Options

-- type WarpLogFunc = (Request -> Status -> Maybe Integer -> IO ())

-- monadLoggerToWarpLogger :: LogFunc -> WarpLogFunc
-- monadLoggerToWarpLogger loggerFunc request status fileSize =
--   let logSource = "warp server"
--       logStr = "\n\t" <> toLogStr (show request)
--         <> "\n\t" <> toLogStr (show status)
--         <> "\n\t" <> toLogStr ("File size: " <> show fileSize)
--   in loggerFunc defaultLoc logSource LevelDebug logStr

-- warpSetting :: LogFunc -> Settings
-- warpSetting logFunc =
--   setLogger (monadLoggerToWarpLogger logFunc) $
--   setPort 8081 defaultSettings

-- type MyAPI = UserAPI :<|> TripAPI :<|> PlaceAPI :<|> MembershipAPI

-- warpApplication :: ConnectionPool -> Application
-- warpApplication pool =
--   simpleCors $
--   provideOptions (Proxy @MyAPI) $
--   serveWithContext (Proxy @MyAPI) EmptyContext $
--   hoistServerWithContext (Proxy @MyAPI) (Proxy @'[]) (flip runReaderT pool) $
--   (postUserServer
--   :<|> (postTripServer :<|> getTripsServer :<|> deleteTripServer)
--   :<|> (postPlaceServer :<|> getPlacesServer)
--   :<|> (postMembershipServer :<|> getMembersServer))

-- warpWebServer :: ConnectionPool -> LoggingT IO ()
-- warpWebServer pool = LoggingT $
--   \logFunc -> runSettings (warpSetting logFunc) $ warpApplication pool
