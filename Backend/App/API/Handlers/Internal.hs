module API.Handlers.Internal
    ( AppM
    ) where

import Control.Monad.Reader
import Database.Persist.Sql
import Servant

type AppM = ReaderT ConnectionPool Handler
