{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Enum where

import Prelude
import Data.Aeson
import Database.Persist.TH
import GHC.Generics


data TransitMode =
  Stay | Walk | Cycle | Motorbike | Bus | Taxi | Drive | Train | Plane | Boat
  deriving (Show, Read, Eq, Generic)

instance ToJSON TransitMode
instance FromJSON TransitMode

derivePersistField "TransitMode"
