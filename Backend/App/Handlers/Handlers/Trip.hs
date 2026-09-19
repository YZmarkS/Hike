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

postTripHandler :: HikeAuthResult -> Trip -> AppM TripId
postTripHandler hikeAuthResult trip = do
  { userId <- extractUserId hikeAuthResult
  ; pool <- asks dbPool
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

getUserTripsHandler :: HikeAuthResult -> AppM [Entity Trip]
getUserTripsHandler hikeAuthResult = do
  { userId <- extractUserId hikeAuthResult
  ; pool <- asks dbPool
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

patchRenameHandler :: HikeAuthResult -> TripId -> Text -> AppM String
patchRenameHandler hikeAuthResult tripId newName = do
  { userId <- extractUserId hikeAuthResult
  ; userId `isOwnerOf` tripId
  ; pool <- asks dbPool
  ; liftIO $ runSqlPool (P.update tripId [ TripName P.=. newName ]) pool
  ; return "Renamed" }

deleteTripHandler :: HikeAuthResult -> TripId -> AppM String
deleteTripHandler hikeAuthResult tripId = do
  { userId <- extractUserId hikeAuthResult
  ; isOwnerOf userId tripId
  ; pool <- asks dbPool
  ; liftIO $ runSqlPool (deleteWhere [TripId P.==. tripId]) pool
  ; return "Deleted" }
