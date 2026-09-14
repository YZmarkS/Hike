{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.Hashing where

import Data.ByteArray
import Data.Text
import Data.Text.Encoding
import Crypto.Error
import Crypto.Random
import Crypto.KDF.Argon2

argonOptions :: Crypto.KDF.Argon2.Options
argonOptions = defaultOptions { variant = Argon2id }

saltLength :: Int
saltLength = 64

hashLength :: Int
hashLength = 64

genSaltAndHash ::
  (MonadRandom m, ByteArray salt, ByteArray out)
  => Text -> m (CryptoFailable (salt, out))
genSaltAndHash password = do
  { let passwordAsBytes = encodeUtf8 password
  ; salt <- getRandomBytes saltLength
  ; let hashResult = hash argonOptions passwordAsBytes salt hashLength
  ; case hashResult of
      CryptoPassed hashedPassword -> return $ return (salt, hashedPassword)
      CryptoFailed err -> return $ CryptoFailed err
  }

hashAndCompare ::
  (ByteArrayAccess salt, ByteArray target)
  => Text -> salt -> target -> Bool
hashAndCompare password salt target =
  let passwordAsBytes = encodeUtf8 password
      hashResult = hash argonOptions passwordAsBytes salt hashLength
  in case hashResult of
       CryptoFailed _ -> False
       CryptoPassed hashedPassword -> hashedPassword == target
