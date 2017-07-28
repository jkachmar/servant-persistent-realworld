{-# LANGUAGE UndecidableInstances #-}

module Logging where

-- Prelude.
import           ClassyPrelude

import           Control.Lens                hiding (scribe)
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8       as BS

-- Metaprogramming imports.
import qualified Data.String.Interpolate     as I
import           Language.Haskell.TH.Quote

-- Logging imports.
import           Control.Monad.Logger        (LogLevel (..), MonadLogger,
                                              monadLoggerLog)
import           Katip                       hiding (Environment)
import           Katip.Monadic
import           System.Log.FastLogger       (fromLogStr, toLogStr)

--------------------------------------------------------------------------------
-- | Type representing logging effects for a @Katip@-powered logger.
newtype LoggingT m a
  = LoggingT
  { unLoggingT :: ReaderT LogState m a
  } deriving ( MonadReader LogState, Functor, Applicative, Monad, MonadIO
             , MonadTrans)

-- | @Katip@ logging information.
data LogState
  = LogState
  { _lsContext   :: !LogContexts
  , _lsNamespace :: !Namespace
  , _lsLogEnv    :: !LogEnv
  }

-- | Generate lenses for @LogState@ as well as the @HasLogState@ class.
makeClassy ''LogState

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
-- | Instance to run functions with @MonadLogger@ baked-in inside @LoggingT@.
instance MonadIO m => MonadLogger (LoggingT m) where
  monadLoggerLog loc _src lvl msg =
    let lvl' = case lvl of
          LevelDebug   -> DebugS
          LevelInfo    -> InfoS
          LevelWarn    -> WarningS
          LevelError   -> ErrorS
          LevelOther _ -> WarningS -- ^ default @LevelOther@ to a warning
        msg' = BS.unpack . fromLogStr . toLogStr $ msg
    in logItemM (Just loc) lvl' (showLS msg')

--------------------------------------------------------------------------------
-- | Merge some context into the log only for the given block.
addContext :: (LogItem i, MonadReader r m, HasLogState r) => i -> m a -> m a
addContext i = local (\r -> r & lsContext <>~ ctxs)
  where ctxs = liftPayload i

-- | Add a layer of namespace to the logs only for the given block.
addNamespace :: (HasLogState r, MonadReader r m) => Namespace -> m a -> m a
addNamespace ns = local (\r -> r & lsNamespace <>~ ns)

-- | Turn off logging only for the given block.
noLogging :: (HasLogState r, MonadReader r m) => m a -> m a
noLogging = local (\r -> r & lsLogEnv %~ clearScribes)

runLoggingT :: LogState -> LoggingT m a -> m a
runLoggingT s = flip runReaderT s . unLoggingT

--------------------------------------------------------------------------------
-- | Loc-tagged logging of debug messages, automatically supplying payload and
-- namespace
logDebugM :: (Applicative m, KatipContext m) => LogStr -> m ()
logDebugM = logLocM DebugS

-- | Loc-tagged logging of info messages, automatically supplying payload and
-- namespace
logInfoM :: (Applicative m, KatipContext m) => LogStr -> m ()
logInfoM = logLocM InfoS

-- | Loc-tagged logging of warning messages, automatically supplying payload
-- and namespace
logWarnM :: (Applicative m, KatipContext m) => LogStr -> m ()
logWarnM = logLocM WarningS

-- | Loc-tagged logging of error messages, automatically supplying payload and
-- namespace
logErrorM :: (Applicative m, KatipContext m) => LogStr -> m ()
logErrorM = logLocM ErrorS

-- | QuasiQuoter for producing an interpolated @Katip@ @LogStr@.
logt :: QuasiQuoter
logt = QuasiQuoter
  { quoteExp  = \s -> [|logStr $(quoteExp I.i $ s)|]
  , quotePat  = err "pattern"
  , quoteType = err "pattern"
  , quoteDec  = err "pattern"
  }
  where
    err name = error $
      "Logger.logt: This QuasiQuoter can not be used as a " <> name <> "!"
