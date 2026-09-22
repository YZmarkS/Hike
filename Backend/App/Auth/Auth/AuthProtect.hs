{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.AuthProtect where

import GHC.Generics

import Auth.JWT
import Control.Monad.IO.Class
import Control.Lens
import Crypto.JWT
import Data.Aeson
import Data.ByteString
import Data.Text.Encoding
import Model
import Servant
import Servant.Foreign
import Servant.Server.Experimental.Auth
import Network.Wai
import Web.Cookie


lookupWith :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupWith _ [] = Nothing
lookupWith f ((a, b) : rest)
  | f a = Just b
  | otherwise = lookupWith f rest

newtype AccessUserId = AccessUserId UserId
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type instance AuthServerData (AuthProtect "hike-jwt-access-auth") = AccessUserId

accessAuthHandlerLogic :: JWK -> Request -> Handler AccessUserId
accessAuthHandlerLogic jwtSigningKey request = do
  { let maybeAccessJWTBytes = do { cookiesBytes <- lookup "Cookie" $ requestHeaders request
                                 ; let cookies = parseCookies cookiesBytes
                                 ; lookup "Hike-Access-JWT" cookies
                                 }
  ; accessJWTBytes <- case maybeAccessJWTBytes of
                        Nothing -> throwError err400
                        Just bytes -> return $ fromStrict bytes
  ; claimsSetResult <- liftIO $ verifyAccessJWT jwtSigningKey accessJWTBytes
  ; accessClaimsSet <- case claimsSetResult of
                   Left _ -> throwError $ err401 { errBody = "Invalid access token" }
                   Right accessClaimsSet' -> return accessClaimsSet'
  ; let maybeSub = view claimSub accessClaimsSet
  ; sub <- case maybeSub of
             Nothing -> throwError $ err401 { errBody = "No sub in token" }
             Just sub' -> return sub'
  ; let maybeSubBody = sub ^? string
  ; subText <- case maybeSubBody of
                 Nothing -> throwError $ err401 { errBody = "sub isn't a string" }
                 Just subText' -> return subText'
  ; let maybeUserId :: Maybe UserId = decodeStrict (encodeUtf8 subText)
  ; userId <- case maybeUserId of
                Nothing -> throwError $ err401 { errBody = "Cannot decode sub" }
                Just userId' -> return userId'
  ; return $ AccessUserId userId
  }

jwkToAccessAuthHandler :: JWK -> AuthHandler Request AccessUserId
jwkToAccessAuthHandler = mkAuthHandler . accessAuthHandlerLogic

instance (HasForeign lang ftype api) =>
  HasForeign lang ftype (AuthProtect sym :> api) where

  type Foreign ftype (AuthProtect sym :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR


newtype RefreshUserId = RefreshUserId UserId

type instance AuthServerData (AuthProtect "hike-jwt-refresh-auth") = RefreshUserId
