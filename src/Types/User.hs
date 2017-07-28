module Types.User where

-- Prelude.
import           ClassyPrelude

import           Control.Lens         (makeFields)
import           Data.Aeson
import           Data.Aeson.Types     (Parser, Options(..), camelTo2)
import           Database.Persist.Sql

-- Local imports.
import           Types.Token          (JWTText)

-- NOTE: All these newtype wrappers might be superfluous; worth thinking on.
--------------------------------------------------------------------------------
-- | Newtype wrapper around @Text@ for a user' email.
newtype UEmail = UEmail Text
  deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show)

-- | Newtype wrapper around @Text@ for a username.
newtype UName = UName Text
  deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show)

-- NOTE: Is this even really necessary? It's nice at least that it doesn't
-- | Newtype wrapper around @Text@ for a user's plaintext password.
newtype UPlainText = UPlainText { fromUPlainText :: Text }
  deriving (Eq, FromJSON)

-- | Newtype wrapper around @Text@ for a user's bio.
newtype UBio = UBio Text
  deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show)

-- | Newtype wrapper around @Text@ for a user's image URL.
newtype UImage = UImage Text
  deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show)

--------------------------------------------------------------------------------
-- | User login JSON request.
data UserLogin
  = UserLogin
  { userLoginEmail    :: !UEmail
  , userLoginPassword :: !UPlainText
  } deriving Generic

instance FromJSON UserLogin where
  parseJSON =
    unwrapUser "UserLogin"
      $ genericParseJSON
      $ camelTrimOpts "userLogin"

--------------------------------------------------------------------------------
-- | User registration JSON request.
data UserRegister
  = UserRegister
  { userRegisterEmail    :: !UEmail
  , userRegisterName     :: !UName
  , userRegisterPassword :: !UPlainText
  } deriving Generic

instance FromJSON UserRegister where
  parseJSON =
    unwrapUser "UserRegister"
      $ genericParseJSON
      $ camelTrimOpts "userRegister"

--------------------------------------------------------------------------------
-- | User update JSON request.
data UserUpdate
  = UserUpdate
  { userUpdateEmail :: !(Maybe UEmail)
  , userUpdateName  :: !(Maybe UName)
  , userPassword    :: !(Maybe UPlainText)
  , userBio         :: !(Maybe UBio)
  , userImage       :: !(Maybe UImage)
  } deriving Generic

instance FromJSON UserUpdate where
  parseJSON =
    unwrapUser "UserUpdate"
      $ genericParseJSON
      $ camelTrimOpts "userUpdate"

--------------------------------------------------------------------------------
-- | User JSON response.
data UserResponse
  = UserResponse
  { userResponseEmail :: !UEmail
  , userResponseToken :: !JWTText
  , userResponseName  :: !UName
  , userResponseBio   :: !(Maybe UBio)
  , userResponseImage :: !(Maybe UImage)
  } deriving Generic

instance ToJSON UserResponse where
  toJSON = wrapUser . (genericToJSON $ camelTrimOpts "userResponse")

--------------------------------------------------------------------------------
-- | Unwrap the top-level "user" object before deserializing some JSON.
unwrapUser :: FromJSON t => String -> (t -> Parser a) -> Value -> Parser a
unwrapUser label parser = withObject label $ \o -> do
  u <- o .: "user"
  parser u

-- | Wrap the data type to-be-serialized in a top-level "user" object.
wrapUser :: ToJSON v => v -> Value
wrapUser nested = object [ "user" .= nested ]

--------------------------------------------------------------------------------
-- | Custom Aeson options for dropping a given string from the field, then
-- either camel or un-camel casing it (encoding or decoding, respectively).
camelTrimOpts :: String -> Options
camelTrimOpts field = defaultOptions { fieldLabelModifier = camelTrim field }

-- | Trim some given string from the field, and then camel or un-camel case it
-- (encoding or decoding, respectively).
camelTrim :: IsSequence String ~ a => String -> String -> String
camelTrim field = camelTo2 '_' . (drop . length $ field)

--------------------------------------------------------------------------------
-- | Generate field accessors.
makeFields ''UserLogin
makeFields ''UserRegister
makeFields ''UserUpdate
