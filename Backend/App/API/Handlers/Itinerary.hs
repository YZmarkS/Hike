{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module API.Handlers.Itinerary
  (
  ) where

import API.Handlers
import Model
import Servant

type PostItineraryItem = Capture "trip_id" TripId :> ReqBody '[JSON] ItineraryItem :> PostCreated '[JSON] ItineraryItemId

-- postItineraryItemServer :: TripId -> ItineraryItem -> AppM ItineraryItemId
-- postItineraryItemServer tripId item =
--     do { pool <- asks id
--        ; let
