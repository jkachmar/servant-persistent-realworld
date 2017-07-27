module App where

-- Prelude.
import           ClassyPrelude               hiding (Handler, keys)

import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp    (run)
import           Configuration.Dotenv        (loadFile, onMissingFile)
import           System.Environment          (lookupEnv)

-- Crypto imports.
import           Crypto.JOSE.JWK             (JWK, fromRSA)
import           Data.X509                   (PrivKey (PrivKeyDSA, PrivKeyRSA))
import           Data.X509.File              (readKeyFile)

-- Servant imports.
import           Servant
import           Servant.Auth.Server         (defaultCookieSettings,
                                              defaultJWTSettings)
-- Local imports.
import           Api                         (api, handler)
import           Foundation
import           Model                       (migrateAll)

--------------------------------------------------------------------------------
-- | Initialize the application and serve the API.
startApp :: IO ()
startApp = do
  -- Load environment variables present in a `.env` file; ignore missing file
  onMissingFile (loadFile False "./.env") (pure ())

  -- Get the app environment.
  env <- lookupSetting "ENV" Development

  -- Get the specified application port, default to 8080 if "APP_PORT" not present
  port <- lookupSetting "APP_PORT" 8080

  -- Loads the RSA keypath into memory; throws an error if the env var is missing
  maybeKeyPath <- lookupEnv "KEYPATH"
  rsaKeyPath   <- case maybeKeyPath of
    Nothing -> throwUserErr "'KEYPATH' not present in environment!"
    Just kp -> pure kp

  -- Generate JWTSettings from an RSA private keyfile; throws an error if the file
  -- is missing
  jwtCfg <- defaultJWTSettings <$> mkJWK rsaKeyPath

  -- Set up Katip logger, dump logs to stdout
  logger <- makeLogger env

  -- Create a 'ConnectionPool', log events with @Katip@
  pool <- makePool env logger

  -- Run database migrations using the connection pool
  runSqlPool (runMigration migrateAll) pool

  let ctx     = Ctx pool jwtCfg 1200 logger
      authCfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      service = enter (appToHandler ctx) handler
      -- The 'Servant' handler for this application

  -- Get this party started!
  liftIO . run port . serveWithContext api authCfg $ service

--------------------------------------------------------------------------------
-- | Look up an environment variable, given a default to fall back to, and
-- `read` that variable into an inferred type.
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
-- 'FilePath'; throws an IO error if no key is present or the first key in the
-- keyfile is a DSA key.
mkJWK :: FilePath -> IO JWK
mkJWK keypath = do
  maybePk <- readKeyFile keypath
  case (headMay maybePk) of
    Nothing ->
      throwUserErr $ "No valid keys present at [" <> show keypath <> "]"
    Just (PrivKeyDSA _) ->
      throwUserErr $ "Invalid key (DSA) present in [" <> show keypath <> "]"
    Just (PrivKeyRSA pk) ->
      pure $ fromRSA pk

--------------------------------------------------------------------------------
throwUserErr :: String -> IO a
throwUserErr = throwIO . userError
