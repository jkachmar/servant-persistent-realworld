module Types.User where

-- Prelude.
import           ClassyPrelude

import           Control.Lens         (makeFields)
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
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
  deriving (Eq, FromJSON, ToJSON)

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
  }

instance FromJSON UserLogin where
  parseJSON = unwrapUser "UserLogin" $
    \u -> UserLogin <$> u .: "email" <*> u .: "password"

--------------------------------------------------------------------------------
-- | User registration JSON request.
data UserRegister
  = UserRegister
  { userRegisterEmail    :: !UEmail
  , userRegisterName     :: !UName
  , userRegisterPassword :: !UPlainText
  }

instance FromJSON UserRegister where
  parseJSON = unwrapUser "UserRegister" $
    \u -> UserRegister <$> u .: "email" <*> u .: "name" <*> u .: "password"

--------------------------------------------------------------------------------
-- | User update JSON request.
data UserUpdate
  = UserUpdate
  { userUpdateEmail :: !(Maybe UEmail)
  , userUpdateName  :: !(Maybe UName)
  , userPassword    :: !(Maybe UPlainText)
  , userBio         :: !(Maybe UBio)
  , userImage       :: !(Maybe UImage)
  }

instance FromJSON UserUpdate where
  parseJSON = unwrapUser "UserUpdate" $
    \u -> UserUpdate <$> u .:? "email" <*> u .:? "name"  <*> u .:? "password"
                     <*> u .:? "bio"   <*> u .:? "image"

--------------------------------------------------------------------------------
-- | User JSON response.
data UserResponse
  = UserResponse
  { userResponseEmail :: !UEmail
  , userResponseToken :: !JWTText
  , userResponseName  :: !UName
  , userResponseBio   :: !(Maybe UBio)
  , userResponseImage :: !(Maybe UImage)
  }

instance ToJSON UserResponse where
  toJSON UserResponse{..} =
    -- Serialize the inner block of the response.
    let nested =
          object [ "email" .= userResponseEmail
                 , "token" .= userResponseToken
                 , "name"  .= userResponseName
                 , "bio"   .= userResponseBio
                 , "image" .= userResponseImage
                 ]
    -- Then wrap that inner block in a "user" object.
    in  object [ "user" .= nested ]

--------------------------------------------------------------------------------
-- | Unwrap the top-level "user" object before deserializing some JSON.
unwrapUser :: FromJSON t => String -> (t -> Parser a) -> Value -> Parser a
unwrapUser label parser = withObject label $ \o -> do
  u <- o .: "user"
  parser u

--------------------------------------------------------------------------------
-- | Generate field accessors.
makeFields ''UserLogin
makeFields ''UserRegister
makeFields ''UserUpdate
