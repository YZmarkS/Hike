{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Membership where

import Auth
import Handlers.Internal
import Control.Monad.Reader
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Experimental
import Model
import Servant
import Types

postNewMembershipServer :: AccessUserId -> TripId -> UserId -> AppM MembershipId
postNewMembershipServer (AccessUserId userId) tripId newUserId = do
  { userId `isOwnerOf` tripId
  ; pool <- asks dbPool
  ; maybeMembershipId <- liftIO $ runSqlPool (insertUnique $ Membership newUserId tripId) pool
  ; case maybeMembershipId of
      Nothing -> throwError $ err409 { errBody = "Debug: Non-unique membership" }
      Just membershipId -> return membershipId }

getTripMembersServer :: AccessUserId -> TripId -> AppM [PublicUserData]
getTripMembersServer (AccessUserId userId) tripId = do
  { userId `isMemberOf` tripId
  ; pool <- asks dbPool
  ; let selectMembers =
            select $ do
              { (membership :& user) <-
                    from $ table @Membership `InnerJoin` table @User
                             `on` (\(membership :& user)
                                       -> membership ^. MembershipUserId
                                          ==. user ^. UserId)
              ; where_ (membership ^. MembershipTripId ==. val tripId)
              ; pure user }
  ; memberEntities <- liftIO $ P.runSqlPool selectMembers pool
  ; return $ map userToPublicUser memberEntities
  }
