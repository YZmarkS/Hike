{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Trip where

import Auth
import Data.Text
import Handlers.Internal
import Control.Monad.Reader
import Control.Exception
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Experimental
import Servant
import Model
import Types

postTripHandler :: AccessUserId -> Trip -> AppM TripId
postTripHandler (AccessUserId userId) trip = do
  { pool <- asks dbPool
  ; let canonicalTrip = trip { tripOwnerId = userId }
  ; let action = do { Just tripId <- P.insertUnique canonicalTrip
                    ; Just _ <- P.insertUnique $ Membership userId tripId
                    ; return $ Just tripId }
  ; maybeTripId <- liftIO $ catch (runSqlPool action pool)
                                  (\(_ :: IOError) -> return Nothing)
  ; case maybeTripId of
      Nothing -> throwError err409
      Just tripId -> return tripId
  }

getAllTripsHandler :: AppM [Entity Trip]
getAllTripsHandler = do
  { pool <- asks dbPool
  ; liftIO $ runSqlPool (P.selectList [] []) pool }

getUserTripsHandler :: AccessUserId -> AppM [Entity Trip]
getUserTripsHandler (AccessUserId userId) = do
  { pool <- asks dbPool
  ; liftIO $ runSqlPool (do { select $ do
                                { (membership :& trip) <-
                                      from $ table @Membership
                                               `InnerJoin` table @Trip
                                               `on` \(membership :& trip) ->
                                                   membership ^. MembershipTripId ==. trip ^. TripId
                                ; where_ (membership ^. MembershipUserId ==. val userId)
                                ; return trip }
                            } ) pool
  }

patchRenameHandler :: AccessUserId -> TripId -> Text -> AppM String
patchRenameHandler (AccessUserId userId) tripId newName = do
  { userId `isOwnerOf` tripId
  ; pool <- asks dbPool
  ; liftIO $ runSqlPool (P.update tripId [ TripName P.=. newName ]) pool
  ; return "Renamed" }

deleteTripHandler :: AccessUserId -> TripId -> AppM String
deleteTripHandler (AccessUserId userId) tripId = do
  { isOwnerOf userId tripId
  ; pool <- asks dbPool
  ; liftIO $ runSqlPool (deleteWhere [TripId P.==. tripId]) pool
  ; return "Deleted" }
