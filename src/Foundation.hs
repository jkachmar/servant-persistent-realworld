module Foundation where

-- Prelude.
import           ClassyPrelude               hiding (Handler, keys)

import           Control.Lens                hiding (scribe)
import           Control.Monad.Except        (ExceptT (..))
import           Control.Monad.Trans.Maybe   (MaybeT (MaybeT), runMaybeT)
import qualified Data.ByteString.Char8       as BS8
import           Data.Time                   (NominalDiffTime)
import           Database.Persist.Postgresql
import           System.Environment          (lookupEnv)

-- Logging imports.
import           Katip                       hiding (Environment)
import qualified Katip                       as K
import           Logging

-- Servant and related imports.
import           Servant                     ((:~>) (NT), Handler (Handler))
import           Servant.Auth.Server         (JWTSettings)

--------------------------------------------------------------------------------
-- | Concrete representation of our app's transformer stack.
type App = AppT IO

-- | Data type representing the most general effects our application should be
-- able to perform.
newtype AppT m a
  = AppT
  { unAppT :: ReaderT Ctx m a
  } deriving ( Functor, Applicative, Monad
             , MonadIO, MonadCatch, MonadThrow
             , MonadReader Ctx
             )

-- | Embed a function from some @Ctx@ to an arbitrary monad in @AppT@.
mkAppT :: (Ctx -> m a) -> AppT m a
mkAppT = AppT . ReaderT

-- | Run an 'AppT' using the given 'Ctx'.
runAppT :: Ctx -> AppT m a -> m a
runAppT ctx app = runReaderT (unAppT app) ctx

--------------------------------------------------------------------------------
-- | Read-only configuration information for our application.
data Ctx
  = Ctx
  { _connPool      :: !ConnectionPool
  , _jwtSettings   :: !JWTSettings
  , _jwtTimeout    :: !NominalDiffTime
  , _katipLogState :: !LogState
  }

-- | Generate lenses for our @Env@.
makeLenses ''Ctx

--------------------------------------------------------------------------------
-- | Implement a @HasLogState@ instance for our @Ctx@ record, allowing the
-- @Katip@ to access the underlying logging context.
instance HasLogState Ctx where
  logState = katipLogState

-- | Implement a @Katip@ instance for our @App@ monad.
instance MonadIO m => Katip (AppT m) where
  getLogEnv = view lsLogEnv

-- | Implement a @KatipContext@ instance for our @App@ monad.
instance MonadIO m => KatipContext (AppT m) where
  getKatipContext   = view lsContext
  getKatipNamespace = view lsNamespace

--------------------------------------------------------------------------------
-- | Convert our 'App' type to a 'Servant.Handler', for a given 'Ctx'.
appToHandler :: Ctx -> App :~> Handler
appToHandler ctx = NT $ Handler . ExceptT . try . runAppT ctx

--------------------------------------------------------------------------------
-- | Application environment.
data Environment
  = Development
  | Testing
  | Staging
  | Production
  deriving (Eq, Read, Show)

--------------------------------------------------------------------------------
-- | Given an @Environment@, return a @LogState@ for our application.
makeLogger :: Environment -> IO LogState
makeLogger env = do
  let katipEnv = K.Environment $ tshow env
  let logLevel = case env of
        Production -> InfoS
        _          -> DebugS

  scribe <- mkHandleScribe ColorIfTerminal stdout logLevel V2
  logger <- initLogEnv mempty katipEnv
  let logEnv = registerScribe "stdout" scribe logger

  pure $ LogState mempty mempty logEnv

--------------------------------------------------------------------------------
-- | Make a Postgresql @ConnectionPool@ for a given environment; logging the
-- events with @Katip@ using the provided @LogState@ context.
makePool :: Environment -> LogState -> IO ConnectionPool
makePool env logger = do
  connStr <- getConnStr env
  let poolSize = getPoolSize env
  let logLoudness = case env of
        Testing -> noLogging
        _       -> id

  runLoggingT logger
    $ logLoudness
    $ addNamespace "psql"
    $ createPostgresqlPool connStr poolSize

-- | Determine the number of connections in our @ConnectionPool@ based on the
-- operating environment.
getPoolSize :: Environment -> Int
getPoolSize Production  = 8
getPoolSize _           = 1

-- | Make a @ConnectionString@ from environment variables, throwing an error if
-- any cannot be found.
getConnStr :: Environment -> IO ConnectionString
getConnStr Development
  = pure $ "host=localhost port=5432 user=jkachmar password= dbname=macchina"
getConnStr Testing
  = pure $ "host=localhost port=5432 user=jkachmar password= dbname=macchina-test"
getConnStr _ = do
  maybePool <- runMaybeT $ do
    let keys = [ "host="
               , " port="
               , " user="
               , " password="
               , " dbname="
               ]
        envs = [ "PGHOST"
               , "PGPORT"
               , "PGUSER"
               , "PGPASS"
               , "PGDATABASE"
               ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    pure $ mconcat . zipWith (<>) keys $ BS8.pack <$> envVars

  case maybePool of
    Nothing ->
      throwIO $ userError $ "Database configuration not present in environment!"
    Just pool -> pure pool
