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

import Data.Time
import Data.Text
import Database.Persist.Sqlite
import Database.Persist.TH

import Model.Enum

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    username Text
    email Text
    UniqueEmail email
    deriving Eq Show Ord

Trip json
    ownerId UserId
    name Text
    UniqueName name
    deriving Eq Show Ord

Goal json
    tripId TripId
    creatorId UserId
    name Text
    note Text Maybe
    achieved Bool
    parentGoalId GoalId Maybe
    deriving Eq Show Ord

Place json
    tripId TripId
    latitude Double
    longitude Double
    name Text Maybe
    note Text Maybe
    UniqueCoordinateInTrip latitude longitude tripId
    deriving Eq Show Ord

ItineraryItem json
    ownerId UserId
    mainPlace PlaceId
    timezone Int
    deriving Eq Show Ord

Route json
    from PlaceId
    to PlaceId
    startTime UTCTime Maybe
    endTime UTCTime Maybe

Leg json
    routeId RouteId
    order Int
    transitMode TransitMode
    deriving Eq Show

PartOf json
    placeId PlaceId
    itineraryId ItineraryItemId
    deriving Eq Show Ord

ContributeTo json
    itineraryId ItineraryItemId
    goalId GoalId
    deriving Eq Show Ord
|]
