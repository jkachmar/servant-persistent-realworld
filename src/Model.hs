module Model
  ( module Model
  , module Persist
  ) where

-- Prelude.
import           ClassyPrelude

import           Control.Lens         (view)
import           Database.Persist.Sql

-- Local imports.
import           Foundation           (Ctx, connPool)

-- Local exports.
import           Model.Persist        as Persist

--------------------------------------------------------------------------------
-- | Constraint for functions that must implement 'MonadIO' and
--  'MonadBaseControl IO'.
type ControlIO m = (MonadIO m, MonadBaseControl IO m)

-- | Type alias for a monad in which we can run DB actions.
type DBM m a = (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

-- | Type alias for a database transaction run in any monad that satisfies the
-- constraints expressed in 'DBM'.
type DB a = forall m. DBM m a

-- | Type alias for a value from a SQL database retrieved with Persistent.
type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

--------------------------------------------------------------------------------
-- | Execute DB action using the connection pool from 'Ctx'; DB actions are
-- executed in a single transaction and rolled back if necessary.
runDB :: (MonadReader Ctx m, MonadIO m) => DB a -> m a
runDB query = do
  pool <- view connPool
  liftIO $ runSqlPool query pool
