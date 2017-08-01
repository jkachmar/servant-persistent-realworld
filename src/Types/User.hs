module Types.User where

-- Prelude.
import           ClassyPrelude

import           Control.Lens         (makeFields)
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
import           Database.Persist.Sql

-- Local imports.
import           Types.Token          (JWTText)
import           Utils.Aeson

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
  parseJSON = unwrapUser $ gCustomParseJSON $ gSnakeCase defaultOptions

--------------------------------------------------------------------------------
-- | User registration JSON request.
data UserRegister
  = UserRegister
  { userRegisterEmail    :: !UEmail
  , userRegisterName     :: !UName
  , userRegisterPassword :: !UPlainText
  } deriving Generic

instance FromJSON UserRegister where
  parseJSON = unwrapUser $ gCustomParseJSON $ gSnakeCase defaultOptions

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
  parseJSON = unwrapUser $ gCustomParseJSON $ gSnakeCase defaultOptions

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
  toJSON = wrapUser . (gCustomToJSON $ gSnakeCase defaultOptions)

--------------------------------------------------------------------------------
-- | Unwrap the top-level "user" object from some JSON before deserializing it.
unwrapUser
  :: forall a b
   . (FromJSON a, Typeable b)
  => (a -> Parser b) -> Value -> Parser b
unwrapUser = unwrapJson "user"

-- | Wrap the data type to-be-serialized in a top-level "user" object.
wrapUser :: ToJSON a => a -> Value
wrapUser = wrapJson "user"

--------------------------------------------------------------------------------
-- | Generate field accessors.
makeFields ''UserLogin
makeFields ''UserRegister
makeFields ''UserUpdate
