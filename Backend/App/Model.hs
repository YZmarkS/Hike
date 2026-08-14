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
    name Text
    UniqueName name
    deriving Eq Show Ord

Goal json
    tripId TripId
    creatorId UserId
    achieved Bool
    parentGoalId GoalId
    deriving Eq Show Ord

Place json
    latitude Double
    longitude Double
    name Text Maybe
    note Text Maybe
    UniqueCoordinate latitude longitude
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
