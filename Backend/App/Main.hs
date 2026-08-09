{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

import           Database.Persist.Sqlite
import           Control.Monad.Logger
import Model

import API.Endpoint.Place

sqliteConnInfo :: SqliteConnectionInfo
sqliteConnInfo = mkSqliteConnectionInfo "data.db"

type WarpLogFunc = (Request -> Status -> Maybe Integer -> IO ())

monadLoggerToWarpLogger :: LogFunc -> WarpLogFunc
monadLoggerToWarpLogger loggerFunc request status fileSize =
  let logSource = "warp server"
      logStr = toLogStr (show request) <> " "
        <> toLogStr (show status) <> " "
        <> toLogStr ("File size: " <> show fileSize)
  in loggerFunc defaultLoc logSource LevelDebug logStr

warpSetting :: LogFunc -> Settings
warpSetting logFunc =
  setLogger (monadLoggerToWarpLogger logFunc) $
  setPort 8081 defaultSettings

warpWebServer :: ConnectionPool -> LoggingT IO ()
warpWebServer pool = LoggingT $
  \logFunc -> runSettings (warpSetting logFunc) $ placeApp pool

main :: IO ()
main = do
  runSqliteInfo sqliteConnInfo $ runMigration migrateAll
  runStderrLoggingT $
    withSqlitePoolInfo sqliteConnInfo 10 warpWebServer
