module Types.Handlers where


import Control.Monad.Reader
import Crypto.JOSE
import Database.Persist.Sql
import Servant


data AppState = MkAppState
  { dbPool :: ConnectionPool
  , jwk :: JWK
  }

type AppM = ReaderT AppState Handler
