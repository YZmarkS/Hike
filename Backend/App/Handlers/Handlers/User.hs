{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.User where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Database.Persist.Sqlite
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
      Nothing -> throwError $ err409 { errBody = "Debug: Non-unique user" }
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
  ; userEntity <- maybe (throwError err401) return maybeUser
  ; let user = entityVal userEntity
        userId = entityKey userEntity
        salt = userSalt user
        hashedPassword = userHashedPassword user
  ; unless ((password, salt) `hashTo` hashedPassword) $ throwError err401
  ; (accessJWT, refreshJWT) <- lift $ generateUserTokens jwk userId
  ; newTokenResult <- liftIO $ runSqlPool (insertBy $ RefreshToken userId refreshJWT False) pool
  ; _ <- either (const $ throwError err409) return newTokenResult
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

{- Refresh Tokens

Generate new access and refresh token.

I have considered whether refreshing should invalidate the old refresh token.
There is a risk of DDoSing the server with refresh request, which will also fill
up the database with refresh tokens.
Given this software is meant to be self-hosted, I don't think this will be an
issue.
-}
postRefreshHandler :: RefreshUserId -> AppM (Headers '[ Header' '[Optional, Strict] "Set-Cookie" SetCookie
                                                      , Header' '[Optional, Strict] "Set-Cookie" SetCookie
                                                      ]
                                             UserId)
postRefreshHandler (RefreshUserId userId) = do
  { jsonWebKey <- asks jwk
  ; pool <- asks dbPool
  ; (accessJWT, refreshJWT) <- lift $ generateUserTokens jsonWebKey userId
  ; let setAccessJWTCookie = defaultSetCookie
                             { setCookieName = "Hike-Access-JWT"
                             , setCookieValue = accessJWT
                             }
        setRefreshJWTCookie = defaultSetCookie
                              { setCookieName = "Hike-Refresh-JWT"
                              , setCookieValue = refreshJWT
                              }
  ; newTokenResult <- liftIO $ runSqlPool (insertBy $ RefreshToken userId refreshJWT False) pool
  ; _ <- either (const $ throwError err409) return newTokenResult
  ; return $ userId
    & addHeader' setAccessJWTCookie
    & addHeader' setRefreshJWTCookie
  }

getAllUsersHandler :: AppM [PublicUserData]
getAllUsersHandler = do
  { pool <- asks dbPool
  ; userEntities <- liftIO $ runSqlPool (selectList [] []) pool
  ; return $ map userToPublicUser userEntities
  }
