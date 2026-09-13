{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.User where

import Handlers.Internal
import Control.Monad.IO.Class
import Control.Monad.Reader

import Database.Persist.Sqlite
import Model
import Servant
import Types
import Types.User
import Crypto.Error
import Auth.Hashing

postUserHandler :: SignUp -> AppM UserId
postUserHandler signUp = do
  { let username = signUpUsername signUp
        email = signUpEmail signUp
        password = signUpPassword signUp
  ; hashResult <- liftIO $ genSaltAndHash password
  ; (salt, hashedPassword) <- case hashResult of
                                CryptoFailed _ -> throwError err500
                                CryptoPassed pair -> return pair

  ; liftIO $ print (salt, hashedPassword)
  ; let newUser = User username email salt hashedPassword
  ; pool <- asks Prelude.id
  ; sqlResult <- liftIO $ runSqlPool (insertUniqueEntity newUser) pool
  ; case sqlResult of
      Nothing -> throwError $ err409 { errBody = "Cannot insert due to uniqueness" }
      Just newUserId -> return $ entityKey newUserId
  }

postLoginHandler :: Login -> AppM UserId
postLoginHandler login = do
  { let email = loginEmail login
        password = loginPassword login
  ; liftIO $ print email
  ; pool <- asks Prelude.id
  ; maybeUser :: Maybe (Entity User) <- liftIO $ runSqlPool (getBy $ UniqueEmail email) pool
  ; userEntity <- case maybeUser of
                    Nothing -> throwError err401
                    Just user -> return user
  ; let user = entityVal userEntity
        salt = userSalt user
        hashedPassword = userHashedPassword user
  ; liftIO $ print (salt, hashedPassword)
  ; if hashAndCompare password salt hashedPassword
    then return $ entityKey userEntity
    else throwError err401
  }


getAllUsersHandler :: AppM [PublicUserData]
getAllUsersHandler = do
  { pool <- asks Prelude.id
  ; userEntities <- liftIO $ runSqlPool (selectList [] []) pool
  ; return $ map userToPublicUser userEntities
  }
