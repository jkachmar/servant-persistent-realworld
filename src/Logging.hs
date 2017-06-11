{-# LANGUAGE UndecidableInstances #-}

module Logging where

-- * Prelude.
import           ClassyPrelude

-- * Base imports.
import           Control.Lens                hiding (scribe)
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8       as BS8

-- * Logging imports.
import           Control.Monad.Logger        (LogLevel (..), MonadLogger,
                                              monadLoggerLog)
import           Katip                       hiding (Environment, LogStr)
import qualified Katip                       as K
import           System.Log.FastLogger       (fromLogStr, toLogStr)

-- * Local project imports.
import           Foundation

--------------------------------------------------------------------------------

-- | Our @LoggingT@ monad, which should be compatible with any @MonadLogger@
-- interface. Not all of the included instances below are strictly necessary,
-- but may be useful for more complex applications, and so have been included.
newtype LoggingT m a
  = LoggingT
  { unLoggingT :: ReaderT LogState m a
  } deriving ( MonadReader LogState, Functor, Applicative, Monad, MonadIO
             , MonadTrans)

instance MonadBase b m => MonadBase b (LoggingT m) where
  liftBase = liftBaseDefault

instance MonadTransControl LoggingT where
  type StT LoggingT a = StT (ReaderT Int) a
  liftWith = defaultLiftWith LoggingT unLoggingT
  restoreT = defaultRestoreT LoggingT

instance MonadBaseControl b m => MonadBaseControl b (LoggingT m) where
  type StM (LoggingT m) a = ComposeSt LoggingT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadIO m => Katip (LoggingT m) where
  getLogEnv = view lsLogEnv

instance MonadIO m => KatipContext (LoggingT m) where
  getKatipContext   = view lsContext
  getKatipNamespace = view lsNamespace

--------------------------------------------------------------------------------
-- | Make @LoggingT@ an instance of @MonadLogger@.
instance MonadIO m => MonadLogger (LoggingT m) where
  monadLoggerLog loc _src lvl msg =
    let lvl' = case lvl of
          LevelDebug   -> DebugS
          LevelInfo    -> InfoS
          LevelWarn    -> WarningS
          LevelError   -> ErrorS
          LevelOther _ -> WarningS -- ^ default @LevelOther@ to a warning
        msg' = BS8.unpack . fromLogStr . toLogStr $ msg
    in logItemM (Just loc) lvl' (showLS msg')

--------------------------------------------------------------------------------
-- | Merge some context into the log only for the given block.
addContext :: (LogItem i, MonadReader r m, HasLogState r) => i -> m a -> m a
addContext i = local (\r -> r & lsContext <>~ ctxs)
  where ctxs = liftPayload i

-- | Add a layer of namespace to the logs only for the given block.
addNamespace :: (HasLogState r, MonadReader r m) => Namespace -> m a -> m a
addNamespace ns = local (\r -> r & lsNamespace <>~ ns)

-- | Turn logging off only for the given block.
noLogging :: (HasLogState r, MonadReader r m) => m a -> m a
noLogging = local (\r -> r & lsLogEnv %~ clearScribes)

runLoggingT :: LogState -> LoggingT m a -> m a
runLoggingT s = flip runReaderT s . unLoggingT

--------------------------------------------------------------------------------
-- | Given an @Environment@, return a @LogEnv@ for our application.
mkLogger :: Environment -> IO LogEnv
mkLogger env = do
  let katipEnv = K.Environment $ tshow env
  let logLevel = case env of
        Production -> InfoS
        _          -> DebugS

  scribe <- mkHandleScribe ColorIfTerminal stdout logLevel V2
  logger <- initLogEnv mempty katipEnv

  pure $ registerScribe "stdout" scribe logger
