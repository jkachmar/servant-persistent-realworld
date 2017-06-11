module Model where

-- * Prelude.
import           ClassyPrelude

-- * Base imports.
import           Control.Lens
import           Data.UUID            (UUID)

-- * Database imports.
import           Database.Persist.Sql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       persistLowerCase, share, sqlSettings)

-- * Local imports.
import           Foundation           (Config, connPool)
import           Types.BCrypt         (BCrypt)
import           Types.Instances      ()
import           Types.User           (UName, UEmail, UBio, UImage)

--------------------------------------------------------------------------------
-- | Constraint for functions that must implement @MonadIO@ and
--  @MonadBaseControl IO@.
type ControlIO m = (MonadIO m, MonadBaseControl IO m)

-- | Type alias for a monad in which we can run DB actions.
type DBM m a = (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

-- | Type alias for a database transaction run in any monad that satisfies the
-- constraints expressed in @DBM@.
type DB a = forall m. DBM m a

-- | Type alias for a value from a SQL database retrieved with Persistent.
type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

--------------------------------------------------------------------------------
-- | Execute read/write DB query using the connection pool from @Config@.
runDB :: (MonadReader Config m, MonadIO m) => DB a -> m a
runDB query = do
  pool <- view connPool
  liftIO $ runSqlPool query pool

--------------------------------------------------------------------------------

share
  [mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
  User sql=users
    name      UName        sqltype=text
    email     UEmail       sqltype=text
    bio       UBio   Maybe sqltype=text
    image     UImage Maybe sqltype=text
    uuid      UUID         sqltype=uuid
    createdAt UTCTime      sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt UTCTime      sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    UniqueEmailUser email
    deriving Eq Generic Show

  Password sql=passwords
    hash      BCrypt
    user      UserId
    createdAt UTCTime sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt UTCTime sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    UniquePasswordUser user
    deriving Eq Generic Show
  |]
