module App where

-- * Prelude
import           ClassyPrelude               hiding (Handler, keys)

-- * Base imports
import           Control.Monad.Trans.Maybe   (MaybeT (MaybeT), runMaybeT)
import qualified Data.ByteString.Char8       as BS
import           Database.Persist.Postgresql (ConnectionString,
                                              createPostgresqlPool,
                                              runMigration, runSqlPool)
import           Network.Wai.Handler.Warp    (run)

-- * Configuration imports
import           Configuration.Dotenv        (loadFile, onMissingFile)
import           System.Environment          (lookupEnv)

-- * Crypto imports.
import           Crypto.JOSE.JWK             (JWK, fromRSA)
import           Data.X509                   (PrivKey (PrivKeyDSA, PrivKeyRSA))
import           Data.X509.File              (readKeyFile)

-- * Servant and related imports.
import           Servant
import           Servant.Auth.Server         (defaultCookieSettings,
                                              defaultJWTSettings)

-- * Local imports.
import           Api
import           Foundation
import           Logging
import           Model                       (migrateAll)

--------------------------------------------------------------------------------
-- | Initialize the application and start serving the API.

startApp :: IO ()
startApp = do
  -- Load environment variables present in a .env file; ignore missing file.
  onMissingFile (loadFile False "./.env") (pure ())

  -- Get the application environment; default to @Development@
  env <- lookupSetting "ENV" Development

  -- Get the port to serve on; default to 8080
  port <- lookupSetting "APP_PORT" 8080

  -- Load the RSA keypath into memory; throw an error if this variable is missing.
  maybeKeyPath <- lookupEnv "KEYPATH"
  rsaKeyPath   <- case maybeKeyPath of
    Nothing -> throwIO $ userError "'KEYPATH' not present in environment!"
    Just kp -> pure kp

  -- Generate @JWTSettings@ from an RSA private keyfile; throws an error if the
  -- file is missing.
  jwtCfg <- defaultJWTSettings <$> mkJWK rsaKeyPath

  -- Initialize the @Katip@ logger, route logs to stdout.
  logEnv <- mkLogger env
  let logger = LogState mempty logEnv mempty

  -- Create a PostgreSQL @ConnectionPool@, logging to the @Katip@ logger.
  connStr <- getConnStr env
  let poolSize = envPool env
  pool <- runLoggingT logger
        $ addNamespace "database"
        $ createPostgresqlPool connStr poolSize

  -- Run database migrations using the connection pool.
  runSqlPool (runMigration migrateAll) pool

  let config  = Config pool jwtCfg logger
      authCfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      service = enter (appToHandler config) server

  liftIO . run port . serveWithContext api authCfg $ service

--------------------------------------------------------------------------------
-- | Look up an environment variable, given a default to fall back to, and
-- 'read' that variable into an inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeEnv <- lookupEnv env
  case maybeEnv of
    Nothing  -> pure def
    Just str -> maybe (handleFailure str) pure (readMay str)
  where
    handleFailure str = error $ mconcat
      [ "Failed to read [["
      , str
      , "]] for environment variable "
      , env
      ]
--------------------------------------------------------------------------------
-- | Creates a JSON Web Key from the first RSA key in the keyfile at the given
-- @FilePath@; throws an IO error if no key is present or the first key in the
-- keyfile is a DSA key.
mkJWK :: FilePath -> IO JWK
mkJWK keypath = do
  maybePk <- readKeyFile keypath
  case (headMay maybePk) of
    Nothing -> throwIO $ userError $ "No valid keys present at [" <> show keypath <> "]"
    Just (PrivKeyDSA _) -> throwIO $ userError $ "Invalid key (DSA) present in [" <> show keypath <> "]"
    Just (PrivKeyRSA pk) -> pure $ fromRSA pk

--------------------------------------------------------------------------------
-- | Make a @ConnectionString@ from environment variables, throwing an error if
-- any cannot be found.
getConnStr :: Environment -> IO ConnectionString
getConnStr Development
  = pure $ "host=localhost port=5432 user=jkachmar password= dbname=macchina"
getConnStr Testing
  = pure $ "host=localhost port=5432 user=test     password= dbname=macchina-test"
getConnStr Staging
  = pure $ "host=localhost port=5432 user=test     password= dbname=macchina-stage"
getConnStr Production = do
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
    pure $ mconcat . zipWith (<>) keys $ BS.pack <$> envVars

  case maybePool of
    Nothing ->
      throwIO $ userError $ "Database configuration not present in environment!"
    Just pool -> pure pool
