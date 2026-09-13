{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.User where

import Handlers.Internal
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import Database.Persist.Sqlite
import Model
import Servant
import Types
import Types.User
import Crypto.Error
import Auth.Hashing
import Auth.JWT

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
  ; pool <- asks Prelude.id
  ; maybeUser :: Maybe (Entity User) <- liftIO $ runSqlPool (getBy $ UniqueEmail email) pool
  ; userEntity <- case maybeUser of
                    Nothing -> throwError err401
                    Just user -> return user
  ; let user = entityVal userEntity
        userId = entityKey userEntity
        salt = userSalt user
        hashedPassword = userHashedPassword user
  ; unless (hashAndCompare password salt hashedPassword) $ throwError err401
  ; (accessJWT, refreshJWT) <- generateTokensForUser _ userId
  ; return $ entityKey userEntity
  }


getAllUsersHandler :: AppM [PublicUserData]
getAllUsersHandler = do
  { pool <- asks Prelude.id
  ; userEntities <- liftIO $ runSqlPool (selectList [] []) pool
  ; return $ map userToPublicUser userEntities
  }
