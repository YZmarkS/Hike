{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Membership where

import Auth
import Handlers.Internal
import Handlers.Internal.Auth
import Control.Monad.Reader
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Experimental
import Model
import Servant
import Types.User

postNewMembershipServer :: HikeAuthResult -> TripId -> UserId -> AppM MembershipId
postNewMembershipServer hikeAuthResult tripId newUserId = do
  { userId <- extractUserId hikeAuthResult
  ; userId `isOwnerOf` tripId
  ; pool <- asks Prelude.id
  ; maybeMembershipId <- liftIO $ runSqlPool (insertUnique $ Membership newUserId tripId) pool
  ; case maybeMembershipId of
      Nothing -> throwError $ err409 { errBody = "Cannot add membership" }
      Just membershipId -> return membershipId }

getTripMembersServer :: HikeAuthResult -> TripId -> AppM [PublicUserData]
getTripMembersServer hikeAuthResult tripId = do
  { userId <- extractUserId hikeAuthResult
  ; userId `isMemberOf` tripId
  ; pool <- asks Prelude.id
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
