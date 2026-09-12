{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson
import Data.Text
import Model

data Signin = MkSignin
  { email :: Text
  , password :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Signin
