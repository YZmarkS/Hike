{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Handlers.Itinerary where

import Model
import Servant

-- type PostItineraryItem = Capture "trip_id" TripId :> ReqBody '[JSON] Itinerary :> PostCreated '[JSON] ItineraryId

-- postItineraryItemServer :: TripId -> ItineraryItem -> AppM ItineraryItemId
-- postItineraryItemServer tripId item =
--     do { pool <- asks id
--        ; let
