{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Database.Persist.Sqlite
import           Control.Monad.Logger
import Model
import WarpApp


sqliteConnInfo :: SqliteConnectionInfo
sqliteConnInfo = mkSqliteConnectionInfo "data.db"

main :: IO ()
main = do
  runSqliteInfo sqliteConnInfo $ runMigration migrateAll
  runStderrLoggingT $
    withSqlitePoolInfo sqliteConnInfo 10 warpWebServer
