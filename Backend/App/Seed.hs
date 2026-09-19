{-# LANGUAGE OverloadedStrings #-}

module Seed
  ( deleteDBFile
  , seed
  ) where

import Config ( sqliteConnInfo )
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Error
import Database.Persist.Sqlite
import Model
import System.Directory
import Types
import Auth.Hashing

signUps :: [SignUp]
signUps =
  [ SignUp "anna@gmail.com" "Anna" "Anna123"
  , SignUp "Frank" "frank@gmail.com" "Frank123"
  , SignUp "Sam" "sam@gmail.com" "Sam123"
  , SignUp "Tim" "tim@gmail.com" "Tim123"
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
  { runMigration migrateAll
  -- Populate users
  ; saltHashPairResults <- liftIO $ mapM (genSaltThenHash . signUpPassword) signUps
  ; saltHashPairs <- liftIO $ mapM throwCryptoErrorIO saltHashPairResults
  ; let userRecords :: [User] = map (\(signUp, (salt, hash)) ->
                                       User { userUsername = signUpUsername signUp
                                            , userEmail = signUpEmail signUp
                                            , userSalt = salt
                                            , userHashedPassword = hash
                                            })
                                $ zip signUps saltHashPairs
  ; userKeys <- insertMany userRecords
  ; let userEntities = zipWith Entity userKeys userRecords
  ; liftIO $ mapM_ print userEntities
  -- Populate trips
  ; let tripRecords = zipWith ($) tripsWithoutOwner userKeys
  ; tripKeys <- insertMany tripRecords
  ; let tripEntities = zipWith Entity tripKeys tripRecords
  ; liftIO $ mapM_ print tripEntities
  -- Make some membership
  ; let membershipRecords = [ Membership (userKeys !! 0) (tripKeys !! 0)
                            , Membership (userKeys !! 2) (tripKeys !! 0)
                            , Membership (userKeys !! 1) (tripKeys !! 1)
                            , Membership (userKeys !! 3) (tripKeys !! 1)
                            ]
  ; membershipKeys <- insertMany membershipRecords
  ; let membershipEntities = zipWith Entity membershipKeys membershipRecords
  ; liftIO $ mapM_ print membershipEntities
  }
