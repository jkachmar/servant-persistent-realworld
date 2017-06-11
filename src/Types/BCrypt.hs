module Types.BCrypt
  ( BCrypt, hashPassword, validatePassword
  ) where

-- * Prelude
import           ClassyPrelude        hiding (hash)

-- * Base imports.
import qualified Crypto.KDF.BCrypt    as BC
import           Database.Persist.Sql

-- * Logging imports.
import           Katip                (KatipContext, Severity (..), logStr,
                                       logTM)

-- * Local imports.
import           Foundation
import           Logging

--------------------------------------------------------------------------------
-- | Newtype wrapper for passwords hashed using BCrypt.
newtype BCrypt
  = BCrypt
  { unBCrypt :: Text
  } deriving (Eq, PersistField, PersistFieldSql, Show)

--------------------------------------------------------------------------------
-- | Produce a hashed output, given some plaintext input.
hashPassword :: MonadIO m => Text -> m BCrypt
hashPassword pass =
  let hash = liftIO $ BC.hashPassword 12 $ encodeUtf8 pass
  in  BCrypt . decodeUtf8 <$> hash

-- | Validate that the plaintext is equivalent to a hashed @BCrypt@, log any
-- validation failures.
validatePassword
  :: (MonadReader r m, HasLogState r, KatipContext m)
  => Text -> BCrypt -> m Bool
validatePassword pass' hash' = do
  let pass    = encodeUtf8 pass'
      hash    = encodeUtf8 . unBCrypt $ hash'
      isValid =  BC.validatePasswordEither pass hash
  case isValid of
    Left e -> addNamespace "password_validation" $ do
      $(logTM) ErrorS
        $  "Password validation failed with [[ "
        <> logStr e
        <> " ]]"
      pure False
    Right v -> pure v
