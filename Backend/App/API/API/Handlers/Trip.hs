{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Trip where
import Auth
import API.Handlers.Internal
import API.Handlers.Internal.Auth
import Data.String
import Data.ByteString.Lazy
import Control.Monad.Reader
import Control.Exception
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Experimental
import Servant
import Model

postTripServer :: HikeAuthResult -> Trip -> AppM TripId
postTripServer hikeAuthResult trip = do
  { userId <- extractUserId hikeAuthResult
  ; pool <- asks id
  ; let canonicalTrip = trip { tripOwnerId = userId }
  ; let action = do { maybeInsertTrip <- P.insertUnique canonicalTrip
                    ; case maybeInsertTrip of
                        Nothing -> return Nothing
                        Just tripId -> do
                          { maybeMembershipId <- P.insertUnique $ Membership userId tripId
                          ; case maybeMembershipId of
                              Nothing -> return Nothing
                              Just _ -> return $ Just tripId } }

  ; maybeTripId <- liftIO $ runSqlPool action pool
  ; case maybeTripId of
      Nothing -> throwError err409
      Just tripId -> return tripId
  }


  -- ; insertTripResult <- liftIO $ runSqlPool (P.insertBy canonicalTrip) pool
  -- ; tripId <- case insertTripResult of
  --               Left trip' -> let errBody = if tripName trip == tripName (entityVal trip')
  --                                           then "Trip with same name already exists"
  --                                           else append
  --                                                    "Cannot insert due to trip id: "
  --                                                    (fromString $ show $ entityKey trip' ) -- is there no better way?
  --                             in throwError $ err409 { errBody }
  --               Right newTripId -> return newTripId
  -- ; insertMembershipResult <- liftIO $ runSqlPool (P.insertBy $ Membership userId tripId) pool
  -- ;

getAllTripsServer :: AppM [Entity Trip]
getAllTripsServer = do
  { pool <- asks id
  ; liftIO $ runSqlPool (P.selectList [] []) pool }

getUserTripsServer :: HikeAuthResult -> AppM [Entity Trip]
getUserTripsServer hikeAuthResult = do
  { userId <- extractUserId hikeAuthResult
  ; pool <- asks id
  ; liftIO $ runSqlPool (do { select $ do
                                { (membership :& trip) <-
                                      from $ table @Membership `InnerJoin` table @Trip
                                               `on` \(membership :& trip) -> membership ^. MembershipTripId ==. trip ^. TripId
                                ; where_ (membership ^. MembershipUserId ==. val userId)
                                ; return trip }
                            } ) pool
  }

deleteTripServer :: HikeAuthResult -> TripId -> AppM String
deleteTripServer hikeAuthResult tripId = do
  { userId <- extractUserId hikeAuthResult
  ; isOwnerOf userId tripId
  ; pool <- asks id
  ; liftIO $ runSqlPool (deleteWhere [TripId P.==. tripId]) pool
  ; return "Deleted" }
