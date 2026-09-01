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
import Servant.Auth.Server as SAS
import Model


userInTrip :: AuthUserId -> TripId -> AppM UserId
userInTrip authUserId tripId = do
  { userId <- extractUserId authUserId
  ; pool <- asks id
  ; maybeMembership <- liftIO $ P.runSqlPool
                       (P.getBy (UniqueUserInTrip userId tripId))
                       pool

  ; case maybeMembership of
      Nothing -> throwError err403
      Just membership -> return $ membershipUserId $ entityVal membership }
userInTrip _ _ = throwError err401

userIsTripOwner :: AuthUserId -> TripId -> AppM Bool
userIsTripOwner authUserId tripId = do
  { userId <- extractUserId authUserId
  ; pool <- asks id
  ; tripResult <- liftIO $ runSqlPool (get tripId) pool
  ; case tripResult of
      Nothing -> throwError err401
      Just trip -> return $ tripOwnerId trip == userId }
userIsTripOwner _ _ = throwError err401

postTripServer :: AuthUserId -> Trip -> AppM TripId
postTripServer authUserId trip = do
  { userId <- extractUserId authUserId
  ; pool <- asks id
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

getUserTripsServer :: AuthUserId -> AppM [Entity Trip]
getUserTripsServer authUserId = do
  { userId <- extractUserId authUserId
  ; pool <- asks id
  ; liftIO $ runSqlPool (do { select $ do
                                { (membership :& trip) <-
                                      from $ table @Membership `InnerJoin` table @Trip
                                               `on` \(membership :& trip) -> membership ^. MembershipTripId ==. trip ^. TripId
                                ; where_ (membership ^. MembershipUserId ==. val userId)
                                ; return trip }
                            } ) pool
  }

deleteTripServer :: AuthUserId -> TripId -> AppM String
deleteTripServer authUserId tripId = do
  { userId <- extractUserId authUserId
  ; liftIO $ print userId
  ; pool <- asks id
  ; userIsTripOwner <- userIsTripOwner authUserId tripId
  ; liftIO $ print userIsTripOwner
  ; if userIsTripOwner
    then liftIO $ runSqlPool (deleteWhere [TripId P.==. tripId]) pool
    else throwError $ err403 { errBody = "You are not the trip owner" }
  ; return "Deleted" }




-- type PostTrip = "trip" :> ReqBody '[JSON] Trip :> PostCreated '[JSON] TripId
-- type GetTrips = "trip" :> Get '[JSON] [Entity Trip]
-- type DeleteTrip = "trip" :> QueryParam' '[Required, Strict] "id" TripId :> Delete '[JSON] String
-- type TripAPI = PostTrip :<|> GetTrips :<|> DeleteTrip
