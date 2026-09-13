module Types.Handlers where

import Control.Monad.Reader
import Database.Persist.Sql
import Servant

type AppM = ReaderT ConnectionPool Handler
