{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Endpoint.Place
  ( PlaceAPI
  , postPlaceServer
  ) where

import Control.Monad.IO.Class
import Database.Persist.Sql
import Servant
import Model

type PostPlace = Capture "trip_id" (Key Trip) :> ReqBody '[JSON] Place :> PostCreated '[JSON] (Key Place)

postPlaceServer :: ConnectionPool -> Server PostPlace
postPlaceServer pool tripId place = do
  sqlResult <- liftIO $ runSqlPool (insertBy place) pool
  case sqlResult of
    Left _ -> throwError $ err409 { errBody = "Place already exists" }
    Right newId -> return newId

type PlaceAPI = PostPlace
