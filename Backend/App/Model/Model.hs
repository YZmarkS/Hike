{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Model where

import Data.ByteString
import Data.Time
import Data.Text
import Database.Persist.Sqlite
import Database.Persist.TH

import Model.Enum

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Text
    email Text
    salt ByteString
    hashedPassword ByteString
    UniqueEmail email
    deriving Eq Show

Trip json
    ownerId UserId
    name Text
    UniqueUserName ownerId name
    deriving Eq Show

Goal json
    tripId TripId
    creatorId UserId
    name Text
    note Text Maybe
    completed Bool
    parentGoalId GoalId Maybe
    deriving Eq Show

Place json
    tripId TripId
    creatorId UserId
    latitude Double
    longitude Double
    name Text Maybe
    note Text Maybe
    UniqueCoordinateInTrip latitude longitude tripId
    deriving Eq Show

ItineraryItem json
    tripId TripId
    creatorId UserId
    mainPlace PlaceId
    startTime UTCTime Maybe
    endTime UTCTime Maybe
    deriving Eq Show

Route json
    from PlaceId
    to PlaceId
    deriving Eq Show

Leg json
    routeId RouteId
    order Int
    transitMode TransitMode
    deriving Eq Show

Membership json
    userId UserId
    tripId TripId OnDeleteCascade
    UniqueUserInTrip userId tripId
    deriving Eq Show

PartOf json
    placeId PlaceId
    itineraryId ItineraryItemId
    deriving Eq Show

ContributeTo json
    itineraryId ItineraryItemId
    goalId GoalId
    deriving Eq Show
|]
