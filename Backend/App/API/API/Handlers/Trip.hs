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

postTripHandler :: HikeAuthResult -> Trip -> AppM TripId
postTripHandler hikeAuthResult trip = do
  { userId <- extractUserId hikeAuthResult
  ; pool <- asks id
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
  { pool <- asks id
  ; liftIO $ runSqlPool (P.selectList [] []) pool }

getUserTripsHandler :: HikeAuthResult -> AppM [Entity Trip]
getUserTripsHandler hikeAuthResult = do
  { userId <- extractUserId hikeAuthResult
  ; pool <- asks id
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
  ; pool <- asks id
  ; liftIO $ runSqlPool (P.update tripId [ TripName P.=. newName ]) pool
  ; return "Renamed" }

deleteTripHandler :: HikeAuthResult -> TripId -> AppM String
deleteTripHandler hikeAuthResult tripId = do
  { userId <- extractUserId hikeAuthResult
  ; isOwnerOf userId tripId
  ; pool <- asks id
  ; liftIO $ runSqlPool (deleteWhere [TripId P.==. tripId]) pool
  ; return "Deleted" }
