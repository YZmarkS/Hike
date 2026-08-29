{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config
import Control.Monad.Logger
import Database.Persist.Sqlite
import Seed
import System.Directory
import ServantApp

main :: IO ()
main = do
  absoluteDBPath <- makeAbsolute sqliteDBPath
  deleteDBFile absoluteDBPath
  seed
  runStderrLoggingT $
    withSqlitePoolInfo sqliteConnInfo 10 application
