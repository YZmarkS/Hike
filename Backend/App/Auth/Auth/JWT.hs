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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.String
import Data.Time
import Model
import Servant

newtype AccessClaimsSet = AccessClaimsSet ClaimsSet
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance HasClaimsSet AccessClaimsSet where
  claimsSet :: Lens' AccessClaimsSet ClaimsSet
  claimsSet f (AccessClaimsSet innerClaimsSet) = fmap AccessClaimsSet (f innerClaimsSet)

newtype RefreshClaimsSet = RefreshClaimsSet ClaimsSet
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance HasClaimsSet RefreshClaimsSet where
  claimsSet :: Lens' RefreshClaimsSet ClaimsSet
  claimsSet f (RefreshClaimsSet innerClaimsSet) = fmap RefreshClaimsSet (f innerClaimsSet)

accessLifespan :: NominalDiffTime
accessLifespan = 60 :: NominalDiffTime

refreshLifespan :: NominalDiffTime
refreshLifespan = 86400 :: NominalDiffTime

mkAccessClaimsSet :: UTCTime -> UserId -> AccessClaimsSet
mkAccessClaimsSet time userId =
  emptyClaimsSet
  & claimSub ?~ fromString (C.unpack $ encode userId)
  & claimAud ?~ Audience ["access"]
  & claimIat ?~ NumericDate time
  & claimExp ?~ NumericDate (addUTCTime accessLifespan time)
  & AccessClaimsSet

mkRefreshClaimsSet :: UTCTime -> UserId -> RefreshClaimsSet
mkRefreshClaimsSet time userId =
  emptyClaimsSet
  & claimSub ?~ fromString (C.unpack $ encode userId)
  & claimAud ?~ Audience ["refresh"]
  & claimIat ?~ NumericDate time
  & claimExp ?~ NumericDate (addUTCTime refreshLifespan time)
  & RefreshClaimsSet

generateJWKForJWT :: MonadRandom m => m JWK
generateJWKForJWT = do
  { genJWK $ OctGenParam 256 }

generateUserTokens :: JWK -> UserId -> Handler (BS.ByteString, BS.ByteString)
generateUserTokens jwk userId = do
  { now <- liftIO getCurrentTime
  ; liftIO $ print (userId)
  ; liftIO $ print ""
  ; liftIO $ print $ "Length of encoding is: "
  ; liftIO $ print $ show $ BSL.length (encode userId)
  ; liftIO $ print (encode userId)
  ; liftIO $ print ""
  ; liftIO $ print $ "Length of show encoding is: "
  ; liftIO $ print $ show $ length (show $ encode userId)
  ; liftIO $ print (show $ encode userId)
  ; liftIO $ print ""
  ; liftIO $ print $ "Length of unpack encoding is: "
  ; liftIO $ print $ show $ length (C.unpack $ encode userId)
  ; liftIO $ print (C.unpack $ encode userId)
  ; liftIO $ print ""
  ; liftIO $ print (fromString (show $ encode userId) :: StringOrURI)
  ; liftIO $ print ""
  ; liftIO $ print ""
  ; liftIO $ print (decode $ C.pack $ show
                    (fromString $ C.unpack $ encode userId :: StringOrURI) :: Maybe UserId)
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


mkVerifier ::
  (HasClaimsSet claims, FromJSON claims)
  => JWTValidationSettings
  -> JWK -> BSL.ByteString -> IO (Either JWTError claims)
mkVerifier jwtValidationSettings jwtSigningKey jwtBytes =
  runJOSE @JWTError $ do
  { accessJWT :: SignedJWT <- decodeCompact jwtBytes
  ; verifyJWT jwtValidationSettings jwtSigningKey accessJWT
  }

verifyAccessJWT :: JWK -> BSL.ByteString -> IO (Either JWTError AccessClaimsSet)
verifyAccessJWT = mkVerifier $ defaultJWTValidationSettings (== "access")

verifyRefreshJWT :: JWK -> BSL.ByteString -> IO (Either JWTError RefreshClaimsSet)
verifyRefreshJWT = mkVerifier $ defaultJWTValidationSettings (== "refresh")
