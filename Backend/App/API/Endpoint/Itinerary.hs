{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module API.Endpoint.Itinerary
  (
  ) where

import Model
import Servant

type PostItineraryItem = Capture "trip_id" TripId :> ReqBody '[JSON] ItineraryItem :> PostCreated '[JSON] ItineraryItemId
