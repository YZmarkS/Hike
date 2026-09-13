{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson
import Data.Text
import Model

data SignUp = SignUp
  { signUpEmail :: Text
  , signUpUsername :: Text
  , signUpPassword :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON SignUp

data Login = Login
  { loginEmail :: Text
  , loginPassword :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Login
