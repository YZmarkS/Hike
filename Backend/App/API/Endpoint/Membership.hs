{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Endpoint.Membership
  ( MembershipAPI
  , postMembershipServer
  , getMembersServer
  ) where

import API.Endpoint.Internal
import Control.Monad.Reader
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Experimental
import Model
import Servant

type PostMembership = "join" :> ReqBody '[JSON] Membership :> PostCreated '[JSON] MembershipId

postMembershipServer :: Membership -> AppM MembershipId
postMembershipServer membership = do
  pool <- asks id
  sqlResult <- liftIO $ runSqlPool (insertUnique membership) pool
  case sqlResult of
    Nothing -> throwError $ err409 { errBody = "Cannot insert due to uniqueness" }
    Just newMembershipId -> return newMembershipId

type GetMembers = Capture "trip_id" TripId :> "members" :> Get '[JSON] [Entity User]

getMembersServer :: TripId -> AppM [Entity User]
getMembersServer tripId =
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

selectMembers tripId = do
  select $ do
    (membership :& user) <-
      from $ table @Membership
      `InnerJoin`
      table @User `on` (\(membership :& user) -> membership ^. MembershipUserId ==. user ^. UserId)
    where_ (membership ^. MembershipTripId ==. tripId)
    pure user

type MembershipAPI = PostMembership :<|> GetMembers
