module Foundation where

-- * Prelude.
import           ClassyPrelude               hiding (Handler)

-- * Base imports.
import           Control.Lens
import           Control.Monad.Except        (MonadError)
import           Database.Persist.Postgresql

-- * Logging imports.
import           Katip                       hiding (Environment)

-- * Servant and related imports.
import           Servant                     ((:~>) (NT), Handler, ServantErr)
import           Servant.Auth.Server         (JWTSettings)

--------------------------------------------------------------------------------
-- | Data type representing the effects we want our application to have.
-- We wrap the standard Servant @Handler@ monad in a @ReaderT Config@
-- transformer, which gives us access to infomration in @Config@ throughout
-- our application using @Control.Lens@'s @view@.
newtype App a
  = App
  { runApp :: ReaderT Config Handler a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Config
             , MonadError ServantErr)

--------------------------------------------------------------------------------
-- | Read-only configuration information for our application.
data Config
  = Config
  { _connPool      :: ConnectionPool
  , _jwtSettings   :: JWTSettings
  , _katipLogState :: LogState
  }

-- | Katip logging information.
data LogState
  = LogState
  { _lsContext   :: LogContexts
  , _lsLogEnv    :: LogEnv
  , _lsNamespace :: Namespace
  }

-- | Classy lenses for @LogState@, which give us accessors as well as the
-- @HasLogState@ typeclass.
makeClassy ''LogState
makeLenses ''Config

--------------------------------------------------------------------------------
-- | Instances required to implement @Katip@ for our @App@ monad.
instance Katip App where
  getLogEnv = view lsLogEnv

instance KatipContext App where
  getKatipContext   = view lsContext
  getKatipNamespace = view lsNamespace

instance HasLogState Config where
  logState = katipLogState

--------------------------------------------------------------------------------
-- | Convert @App@ to a Servant @Handler@, for a given @Config@.
appToHandler :: Config -> App :~> Handler
appToHandler cfg = NT $ flip runReaderT cfg . runApp

--------------------------------------------------------------------------------
-- | Application environment.
data Environment
  = Development
  | Testing
  | Staging
  | Production
  deriving (Eq, Read, Show)

-- | Determine the number of connections in our @ConnectionPool@ based on the
-- operating environment.
envPool :: Environment -> Int
envPool Production  = 8
envPool _           = 1
