{-# LANGUAGE DeriveGeneric #-}

module Types.Auth where

import GHC.Generics
import Data.Aeson
import Model

data JWTSub = JWTSub
  { userId :: UserId
  } deriving (Show, Eq, Generic)

instance ToJSON JWTSub
instance FromJSON JWTSub
