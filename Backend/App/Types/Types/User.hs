{-# LANGUAGE DeriveGeneric #-}
module Types.User where

import GHC.Generics

import Data.Aeson
import Data.Text
import Database.Persist
import Model

data PublicUserData = PublicUserData
  { id :: UserId
  , email :: Text
  , username :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON PublicUserData
instance FromJSON PublicUserData

userToPublicUser :: Entity User -> PublicUserData
userToPublicUser userEntity =
  PublicUserData { Types.User.id = entityKey userEntity
                 , email = userEmail $ entityVal userEntity
                 , username = userUsername $ entityVal userEntity
                 }
