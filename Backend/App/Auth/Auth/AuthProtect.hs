{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.AuthProtect where

import GHC.Generics

import Auth.JWT
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Crypto.JWT
import Data.Aeson
import Data.ByteString
import Data.Text.Encoding
import Database.Persist.Sqlite
import Model
import Servant
import Servant.Foreign
import Servant.Server.Experimental.Auth
import Network.Wai
import Web.Cookie

getCookieByCookieName :: ByteString -> Request -> Maybe ByteString
getCookieByCookieName cookieName request = do
  { cookiesBytes <- lookup "Cookie" $ requestHeaders request
  ; let cookies = parseCookies cookiesBytes
  ; lookup cookieName cookies
  }

lookupWith :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupWith _ [] = Nothing
lookupWith f ((a, b) : rest)
  | f a = Just b
  | otherwise = lookupWith f rest

newtype AccessUserId = AccessUserId UserId
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type instance AuthServerData (AuthProtect "hike-access-jwt-auth") = AccessUserId

accessAuthHandler :: JWK -> Request -> Handler AccessUserId
accessAuthHandler jsonWebKey request = do
  { let maybeAccessJWTBytes = getCookieByCookieName "Hike-Access-JWT" request
  ; accessJWTBytes <- maybe (throwError err400) (return . fromStrict) maybeAccessJWTBytes
  ; claimsSetResult <- liftIO $ verifyAccessJWT jsonWebKey accessJWTBytes
  ; accessClaimsSet <- either
                       (const $ throwError $ err401 { errBody = "Invalid access token" })
                       return
                       claimsSetResult
  ; let maybeUserId :: Maybe UserId = do { subContent <- view claimSub accessClaimsSet
                                         ; subText <- preview string subContent
                                         ; decodeStrict $ encodeUtf8 subText
                                         }
  ; userId <- maybe
              (throwError $ err401 { errBody = "Cannot decode sub" })
              return
              maybeUserId
  ; return $ AccessUserId userId
  }

mkAccessAuthHandler :: JWK -> AuthHandler Request AccessUserId
mkAccessAuthHandler = mkAuthHandler . accessAuthHandler

newtype RefreshUserId = RefreshUserId UserId
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type instance AuthServerData (AuthProtect "hike-refresh-jwt-auth") = RefreshUserId

refreshAuthHandler :: ConnectionPool -> JWK -> Request -> Handler RefreshUserId
refreshAuthHandler pool jsonWebKey request = do
  { let maybeRefreshJWTBytes = getCookieByCookieName "Hike-Refresh-JWT" request
  ; refreshJWTBytes <- maybe (throwError err400) (return . fromStrict) maybeRefreshJWTBytes
  ; claimsSetResult <- liftIO $ verifyRefreshJWT jsonWebKey refreshJWTBytes
  ; refreshClaimsSet <- either
                        (const $ throwError $ err401 {errBody = "Invalid refresh token"})
                        return
                        claimsSetResult
  ; let maybeUserId :: Maybe UserId = do { subContent <- view claimSub refreshClaimsSet
                                         ; subText <- preview string subContent
                                         ; decodeStrict $ encodeUtf8 subText
                                         }
  ; userId <- maybe
              (throwError $ err401 { errBody = "Cannot decode sub" })
              return
              maybeUserId

  ; maybeRefreshTokenRow  <-
      liftIO $ runSqlPool (getBy $ UniqueUserIdToken userId (toStrict refreshJWTBytes)) pool
  ; refreshTokenRow <- maybe (throwError $ err401 { errBody = "No refresh JWT found" })
                       (return . entityVal)
                       maybeRefreshTokenRow
  ; when (refreshTokenRevoked refreshTokenRow)
    (throwError $ err401 { errBody = "Token revoked" })
  ; return $ RefreshUserId userId
  }

mkRefreshAuthHandler :: ConnectionPool -> JWK -> AuthHandler Request RefreshUserId
mkRefreshAuthHandler dbPool jsonWebKey = mkAuthHandler $ refreshAuthHandler dbPool jsonWebKey


instance (HasForeign lang ftype api) =>
  HasForeign lang ftype (AuthProtect sym :> api) where

  type Foreign ftype (AuthProtect sym :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR
