module Config where

import Data.Text
import Database.Persist.Sqlite

sqliteDBPath :: String
sqliteDBPath = "data.db"

sqliteConnInfo :: SqliteConnectionInfo
sqliteConnInfo = mkSqliteConnectionInfo (pack sqliteDBPath)
