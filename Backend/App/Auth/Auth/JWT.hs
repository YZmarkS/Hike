{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Auth.JWT where

import GHC.Generics

import Control.Lens
import Control.Monad.Trans
import Crypto.JOSE.JWK
import Crypto.JWT
import Data.Aeson
import qualified Data.ByteString as BS
import Data.String
import Data.Time
import Model
import Servant
import Servant.Server.Experimental.Auth
import Types


newtype AccessClaimsSet = AccessClaimsSet ClaimsSet
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance HasClaimsSet AccessClaimsSet where
  claimsSet f (AccessClaimsSet innerClaimsSet) = fmap AccessClaimsSet (f innerClaimsSet)

newtype RefreshClaimsSet = RefreshClaimsSet ClaimsSet
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance HasClaimsSet RefreshClaimsSet where
  claimsSet f (RefreshClaimsSet innerClaimsSet) = fmap RefreshClaimsSet (f innerClaimsSet)

accessLifespan :: NominalDiffTime
accessLifespan = 5 :: NominalDiffTime

refreshLifespan :: NominalDiffTime
refreshLifespan = 86400 :: NominalDiffTime

mkAccessClaimsSet :: UTCTime -> UserId -> AccessClaimsSet
mkAccessClaimsSet time userId =
  emptyClaimsSet
  & claimSub ?~ fromString (show $ encode userId)
  & claimAud ?~ Audience ["access"]
  & claimIat ?~ NumericDate time
  & claimExp ?~ NumericDate (addUTCTime accessLifespan time)
  & AccessClaimsSet

mkRefreshClaimsSet :: UTCTime -> UserId -> RefreshClaimsSet
mkRefreshClaimsSet time userId =
  emptyClaimsSet
  & claimSub ?~ fromString (show $ encode userId)
  & claimAud ?~ Audience ["refresh"]
  & claimIat ?~ NumericDate time
  & claimExp ?~ NumericDate (addUTCTime refreshLifespan time)
  & RefreshClaimsSet

generateJWKForJWT :: MonadRandom m => m JWK
generateJWKForJWT = do
  { genJWK $ OctGenParam 256 }

generateUserTokens :: JWK -> UserId -> AppM (BS.ByteString, BS.ByteString)
generateUserTokens jwk userId = do
  { now <- liftIO getCurrentTime
  ; let accessClaimsSet = mkAccessClaimsSet now userId
        refreshClaimsSet = mkRefreshClaimsSet now userId
        jwsHeader = newJWSHeaderProtected HS256
  ; accessJWTResult <- liftIO $ runJOSE @JWTError (signJWT jwk jwsHeader accessClaimsSet)
  ; accessJWT <- case accessJWTResult of
                         Left _ -> throwError err500
                         Right token -> return token
  ; refreshJWTResult <- liftIO $ runJOSE @JWTError (signJWT jwk jwsHeader refreshClaimsSet)
  ; refreshJWT <- case refreshJWTResult of
                          Left _ -> throwError err500
                          Right token -> return token
  ; return ( BS.toStrict $ encodeCompact accessJWT
           , BS.toStrict $ encodeCompact refreshJWT)
  }

type instance AuthServerData (AuthProtect "hike-jwt-access-auth") = AccessClaimsSet
type instance AuthServerData (AuthProtect "hike-jwt-refresh-auth") = RefreshClaimsSet
