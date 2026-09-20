{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.User where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Database.Persist.Sqlite
import Handlers.Internal
import Model
import Servant
import Types
import Crypto.Error
import Auth
import Web.Cookie

postUserHandler :: SignUp -> AppM UserId
postUserHandler signUp = do
  { let username = signUpUsername signUp
        email = signUpEmail signUp
        password = signUpPassword signUp
  ; hashResult <- liftIO $ genSaltThenHash password
  ; (salt, hashedPassword) <- case hashResult of
                                CryptoFailed _ -> throwError err500
                                CryptoPassed pair -> return pair
  ; let newUser = User username email salt hashedPassword
  ; pool <- asks dbPool
  ; sqlResult <- liftIO $ runSqlPool (insertUniqueEntity newUser) pool
  ; case sqlResult of
      Nothing -> throwError $ err409 { errBody = "Cannot insert due to uniqueness" }
      Just newUserId -> return $ entityKey newUserId
  }

postLoginHandler :: Login -> AppM (Headers '[ Header' '[Optional, Strict] "Set-Cookie" SetCookie
                                            , Header' '[Optional, Strict] "Set-Cookie" SetCookie
                                            ]
                                   UserId)
postLoginHandler login = do
  { let email = loginEmail login
        password = loginPassword login
  ; pool <- asks dbPool
  ; jwk <- asks jwk
  ; maybeUser :: Maybe (Entity User) <- liftIO $ runSqlPool (getBy $ UniqueEmail email) pool
  ; userEntity <- case maybeUser of
                    Nothing -> throwError err401
                    Just user -> return user
  ; let user = entityVal userEntity
        userId = entityKey userEntity
        salt = userSalt user
        hashedPassword = userHashedPassword user
  ; unless (hashAndCompare password salt hashedPassword) $ throwError err401
  ; (accessJWT, refreshJWT) <- lift $ generateUserTokens jwk userId
  ; _ <- upsertRefreshJWT userId refreshJWT
  ; let setAccessJWTCookie = defaultSetCookie
                             { setCookieName = "Hike-Access-JWT"
                             , setCookieValue = accessJWT
                             }
        setRefreshJWTCookie = defaultSetCookie
                              { setCookieName = "Hike-Refresh-JWT"
                              , setCookieValue = refreshJWT
                              }
  ; return $ userId
    & addHeader' setAccessJWTCookie
    & addHeader' setRefreshJWTCookie
  }

getNothingHandler :: AccessUserId -> AppM NoContent
getNothingHandler _ = return NoContent

getAllUsersHandler :: AppM [PublicUserData]
getAllUsersHandler = do
  { pool <- asks dbPool
  ; userEntities <- liftIO $ runSqlPool (selectList [] []) pool
  ; return $ map userToPublicUser userEntities
  }
