{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Control.Monad
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Text

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

newtype ResultT m a =
  ResultT { runResultT :: m (Result a) }

unwrappedFmap ::
  Functor m
  => (a -> b) -> m (Result a) -> m (Result b)
unwrappedFmap f = fmap (fmap f)

instance Functor m => Functor (ResultT m) where
  fmap :: (a -> b) -> ResultT m a -> ResultT m b
  fmap f = ResultT . unwrappedFmap f . runResultT

instance (Functor m, Monad m) => Applicative (ResultT m) where
  pure :: a -> ResultT m a
  pure = ResultT . return . Success

  (<*>) :: ResultT m (a -> b) -> ResultT m a -> ResultT m b
  mf <*> ma = ResultT $ do
    { resultF <- runResultT mf
    ; case resultF of
        Error err -> return $ Error err
        Success f -> do { resultA <- runResultT ma
                        ; case resultA of
                            Error err' -> return $ Error err'
                            Success a -> return $ Success (f a)
                        }
    }

instance Monad m => Monad (ResultT m) where
  return :: a -> ResultT m a
  return = ResultT . return . Success

  (>>=) :: ResultT m a -> (a -> ResultT m b) -> ResultT m b
  action >>= f = ResultT $ do
    { result <- runResultT action
    ; case result of
        Error err -> return (Error err)
        Success x -> runResultT $ f x
    }

instance MonadTrans ResultT where
  lift = ResultT . liftM Success
