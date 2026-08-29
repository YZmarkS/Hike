{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Trip where

import API.Handlers.Internal
import Auth
import Data.String
import Data.ByteString.Lazy
import Control.Monad.Reader
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Experimental
import Servant
import Model

userInTrip :: AuthenticatedUser -> TripId -> AppM Bool
userInTrip (AuthenticatedUser { userId }) tripId = do
  { pool <- asks id
  ; memberships :: [Entity Membership] <- liftIO $ P.runSqlPool (P.selectList
                                                                      [ MembershipUserId P.==. userId
                                                                      , MembershipTripId P.==. tripId ] []) pool
  ; return $ not (Prelude.null memberships) }

userIsTripOwner :: AuthenticatedUser -> TripId -> AppM Bool
userIsTripOwner (AuthenticatedUser { userId }) tripId = do
  { pool <- asks id
  ; tripResult <- liftIO $ runSqlPool (get tripId) pool
  ; case tripResult of
      Nothing -> return False
      Just trip -> return $ tripOwnerId trip == userId }

postTripServer :: AuthenticatedUser -> Trip -> AppM TripId
postTripServer (AuthenticatedUser { userId }) trip = do
  { pool <- asks id
  ; let canonicalTrip = trip { tripOwnerId = userId }
  ; insertTripResult <- liftIO $ runSqlPool (P.insertBy canonicalTrip) pool
  ; tripId <- case insertTripResult of
                Left trip' -> let errBody = if tripName trip == tripName (entityVal trip')
                                            then "Trip with same name already exists"
                                            else append
                                                     "Cannot insert due to trip id: "
                                                     (fromString $ show $ entityKey trip' ) -- is there no better way?
                              in throwError $ err409 { errBody }
                Right newTripId -> return newTripId
  ; insertMembershipResult <- liftIO $ runSqlPool (P.insertBy $ Membership userId tripId) pool
  ; case insertMembershipResult of
      Left _ -> throwError err500
      Right _ -> return tripId }

getAllTripsServer :: AppM [Entity Trip]
getAllTripsServer = do
  { pool <- asks id
  ; liftIO $ runSqlPool (P.selectList [] []) pool }

getUserTripsServer :: AuthenticatedUser -> AppM [Entity Trip]
getUserTripsServer (AuthenticatedUser { userId }) = do
  { pool <- asks id
  ; liftIO $ runSqlPool (do { select $ do
                                { (membership :& trip) <-
                                      from $ table @Membership `InnerJoin` table @Trip
                                               `on` \(membership :& trip) -> membership ^. MembershipTripId ==. trip ^. TripId
                                ; where_ (membership ^. MembershipUserId ==. val userId)
                                ; return trip }
                            } ) pool
  }

deleteTripServer :: AuthenticatedUser -> TripId -> AppM String
deleteTripServer authUser tripId = do
  { pool <- asks id
  ; userIsTripOwner <- userIsTripOwner authUser tripId
  ; if userIsTripOwner
    then throwError $ err403 { errBody = "You are not the trip owner" }
    else liftIO $ runSqlPool (deleteWhere [TripId P.==. tripId]) pool
  ; return "Deleted" }




-- type PostTrip = "trip" :> ReqBody '[JSON] Trip :> PostCreated '[JSON] TripId
-- type GetTrips = "trip" :> Get '[JSON] [Entity Trip]
-- type DeleteTrip = "trip" :> QueryParam' '[Required, Strict] "id" TripId :> Delete '[JSON] String
-- type TripAPI = PostTrip :<|> GetTrips :<|> DeleteTrip
