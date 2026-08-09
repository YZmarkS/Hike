{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Endpoint.Place
  ( placeApp
  ) where

import Control.Monad.IO.Class
import Database.Persist.Sql
import Servant
import Model

type PutPlace = "place" :> ReqBody '[JSON] Place :> PostCreated '[JSON] (Key Place)

putPlaceServer :: ConnectionPool -> Server PutPlace
putPlaceServer pool place = do
  sqlResult <- liftIO $ runSqlPool (insertBy place) pool
  case sqlResult of
    Left _ -> throwError $ err409 { errBody = "Place already exists" }
    Right newId -> return newId

type PlaceAPI = PutPlace

placeApp :: ConnectionPool -> Application
placeApp pool = serve (Proxy :: Proxy PlaceAPI) (putPlaceServer pool)



--   let action = do
--         sqlResult <- insertBy place
--         return (case sqlResult of
--                   Left _ -> True
--                   Right _ -> True)
--   in runSqlPool action pool

-- putPlaceServer' :: ConnectionPool -> Server PutPlace
-- putPlaceServer' pool place = do
--   sqlResult <- liftIO $ runSqlPool (insertBy place) pool
--   case sqlResult of
--     Left _ -> throwError err409
--     Right _ -> return True
