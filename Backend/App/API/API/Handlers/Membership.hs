{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Membership where

import Auth
import API.Handlers.Internal
import API.Handlers.Internal.Auth
import Control.Monad.Reader
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Experimental
import Model
import Servant

postNewMembershipServer :: HikeAuthResult -> TripId -> UserId -> AppM MembershipId
postNewMembershipServer hikeAuthResult tripId newUserId = do
  { userId <- extractUserId hikeAuthResult
  ; pool <- asks id
  ; maybeInsert <- liftIO $ runSqlPool (insertUnique $ Membership newUserId tripId) pool
  ; case maybeInsert of
      Nothing -> throwError $ err409 { errBody = "Invalid membership" }
      Just newMembershipId -> return newMembershipId }

getTripMembersServer :: HikeAuthResult -> TripId -> AppM [Entity User]
getTripMembersServer hikeAuthResult tripId =
    do { pool <- asks id
       ; maybeTrip <- liftIO $ P.runSqlPool (get tripId) pool
       ; trip <- case maybeTrip of
                   Nothing -> throwError $ err404 { errBody = "Cannot found trip" }
                   Just trip -> return trip
       ; let ownerId = tripOwnerId trip
       ; maybeOwner <- liftIO $ P.runSqlPool (getEntity ownerId) pool
       ; owner <- case maybeOwner of
                    Nothing -> throwError $ err500 { errBody = "Internal data error" }
                    Just owner -> return owner
       ; members <- liftIO $ P.runSqlPool (selectMembers (val tripId)) pool
       ; return (owner : members) }
    where
      selectMembers tripId =
          select $ do
                 { (membership :& user) <-
                   from $ table @Membership
                   `InnerJoin`
                   table @User `on` (\(membership :& user) -> membership ^. MembershipUserId ==. user ^. UserId)
                 ; where_ (membership ^. MembershipTripId ==. tripId)
                 ; pure user }

-- type DeleteMembership = Capture "trip_id" TripId :> "members

-- type MembershipAPI = PostMembership :<|> GetMembers
