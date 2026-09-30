module Handlers.Internal.Auth where

import Control.Monad.Reader
import Data.ByteString
import Database.Persist.Sql
import Model
import Types

-- upsertRefreshJWT :: UserId -> ByteString -> AppM (Entity RefreshToken)
-- upsertRefreshJWT userId refreshJWT = do
--   { let record = RefreshToken userId refreshJWT False
--   ; pool <- asks dbPool
--   ; let upsertAction = upsertBy
--                        (UniqueUserIdToken userId refreshJWT)
--                        record
--                        [RefreshTokenRefreshJwt =. refreshJWT]
--   ; liftIO $ runSqlPool upsertAction pool
-- }
