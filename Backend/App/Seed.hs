{-# LANGUAGE OverloadedStrings #-}

module Seed
  ( deleteDBFile
  , seed
  ) where

import Config
import Control.Monad
import Control.Monad.IO.Class
import Database.Persist.Sqlite
import Model
import System.Directory

userRecords :: [User]
userRecords =
  [ User "Anna" "anna@gmail.com"
  , User "Frank" "frank@gmail.com"
  , User "Sam" "sam@gmail.com"
  , User "Tim" "tim@gmail.com"
  ]

tripsWithoutOwner :: [UserId -> Trip]
tripsWithoutOwner =
  [ flip Trip "Canada"
  , flip Trip "Japan"
  ]

deleteDBFile :: FilePath -> IO ()
deleteDBFile dbPath = do
  print dbPath
  exist <- doesFileExist dbPath
  print exist
  when exist $ do
    removeFile dbPath
    let walPath = dbPath ++ "-wal"
    let shmPath = dbPath ++ "-shm"
    walExists <- doesFileExist walPath
    shmExists <- doesFileExist shmPath
    when walExists $ removeFile walPath
    when shmExists $ removeFile shmPath

seed :: IO ()
seed = runSqliteInfo sqliteConnInfo $ do
  runMigration migrateAll
  -- Populate users
  userKeys <- insertMany userRecords
  let userEntities = zipWith Entity userKeys userRecords
  liftIO $ mapM_ print userEntities
  -- Populate trips
  let tripRecords = zipWith ($) tripsWithoutOwner userKeys
  tripKeys <- insertMany tripRecords
  let tripEntities = zipWith Entity tripKeys tripRecords
  liftIO $ mapM_ print tripEntities
  -- Make some membership
  let membershipRecords = [ Membership (userKeys !! 0) (tripKeys !! 0)
                          , Membership (userKeys !! 2) (tripKeys !! 0)
                          , Membership (userKeys !! 1) (tripKeys !! 1)
                          , Membership (userKeys !! 3) (tripKeys !! 1)
                          ]
  membershipKeys <- insertMany membershipRecords
  let membershipEntities = zipWith Entity membershipKeys membershipRecords
  liftIO $ mapM_ print membershipEntities
