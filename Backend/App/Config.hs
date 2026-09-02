{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad.Logger
import Data.Text
import Database.Persist.Sqlite
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

-- SQLite Configuration
sqliteDBPath :: String
sqliteDBPath = "data.db"

sqliteConnInfo :: SqliteConnectionInfo
sqliteConnInfo = mkSqliteConnectionInfo (pack sqliteDBPath)

-- Warp Server Configuration
type WarpLogFunc = (Request -> Status -> Maybe Integer -> IO ())

monadLoggerToWarpLogger :: LogFunc -> WarpLogFunc
monadLoggerToWarpLogger logFunc request status fileSize =
  let logSource = "warp server"
      logStr = "\n\t" <> toLogStr (show request) <>
               "\n\t" <> toLogStr (show status) <>
               "\n\t" <> toLogStr ("File size: " <> show fileSize)
  in logFunc defaultLoc logSource LevelDebug logStr

hikeWarpSetting :: LogFunc -> Settings
hikeWarpSetting logFunc =
  setLogger (monadLoggerToWarpLogger logFunc) $
  setPort 8081 defaultSettings


runAppWithWarp :: Application -> LoggingT IO ()
runAppWithWarp app = LoggingT $ \logFunc -> runSettings (hikeWarpSetting logFunc) app
