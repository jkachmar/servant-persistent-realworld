module Api.Users where

-- Prelude.
import           ClassyPrelude hiding (hash)

import           Control.Lens
import           Data.Time           (NominalDiffTime, addUTCTime)
import           Database.Persist    (Entity(Entity))

-- Servant imports.
import           Servant
import           Servant.Auth.Server hiding (makeJWT)

-- Local imports.
import           Foundation
import           Logging
import           Model
import           Query.User
import           Types.BCrypt
import           Types.Token
import           Types.User          hiding (userBio, userImage)

--------------------------------------------------------------------------------
-- | Servant type-level representation of the "users" route fragment.
type UsersApi auths = (Auth auths Token :> ProtectedApi) :<|> UnprotectedApi

-- | Handler function for the "users" route fragment.
usersHandler :: ServerT (UsersApi auths) App
usersHandler = protected :<|> unprotected

--------------------------------------------------------------------------------
-- | Type-level representation of the endpoints protected by 'Auth'.
type ProtectedApi =
       "users"
         :> "register"
           :> ReqBody '[JSON] UserRegister
             :> Post '[JSON] UserResponse

-- | Check authentication status and dispatch the request to the appropriate
-- endpoint handler.
protected :: AuthResult Token -> ServerT ProtectedApi App
protected (Authenticated t) = register t
protected _                 = pure $ throwM err401

-- | Registration endpoint handler.
register :: Token -> UserRegister -> App UserResponse
register _ userReg = do
  hashedPw <- hashPassword $ fromUPlainText $ userReg ^. password
  dbUser   <- runDB $ insertUser (userReg ^. name) (userReg ^. email) hashedPw
  let logAction = addNamespace "register"
                $ logInfoM [logt|"#{dbUser} was registered."|]
  mkUserResponse userReg hashedPw dbUser logAction

--------------------------------------------------------------------------------
-- | Type-level representation of the endpoints not protected by 'Auth'.
type UnprotectedApi =
       "users"
         :> "login"
           :> ReqBody '[JSON] UserLogin
             :> Post '[JSON] UserResponse

-- | Dispatch the request to the appropriate endpoint handler.
unprotected :: ServerT UnprotectedApi App
unprotected = login

login :: UserLogin -> App UserResponse
login userLogin = do
  -- Get the user and password associated with this email, if they exist.
  maybeUserPass    <- runDB $ getUserByEmail (userLogin ^. email)
  (dbUser, dbPass) <- case maybeUserPass of
    Nothing -> throwM err404
    Just ( (Entity _ dbUser)
         , (Entity _ dbPass)) -> pure (dbUser, dbPass)

  let logAction = addNamespace "login"
                $ logInfoM [logt|"#{dbUser} logged in."|]

  mkUserResponse userLogin (passwordHash dbPass) dbUser logAction

--------------------------------------------------------------------------------
-- | Return a token for a given user if the login password is valid when
-- compared to the hash in the database; throw 401 if the user's password
-- is invalid
mkToken :: Text -> BCrypt -> User -> App Token
mkToken pass hashed dbUser = do
  -- Validate the stored hash against the plaintext password
  isValid <- validatePassword pass hashed

  -- If the password isn't valid, throw a 401
  -- TODO - maybe validatePassword should return an Either so that when
  -- validation fails internally, we can throw a 500.
  if isValid then pure () else throwM err401
  pure $ Token (userUuid dbUser)

-- | Return a textual view of a JWT from a token, valid for a given duration
-- of seconds
mkJWT :: Token -> NominalDiffTime -> App JWTText
mkJWT token duration = do
  -- Try to make a JWT with the settings from the Reader environment, with an
  -- expiry time 1 hour from now
  settings <- view jwtSettings
  expires  <- liftIO $ Just . (addUTCTime duration) <$> getCurrentTime
  tryJWT   <- liftIO $ makeJWT token settings expires

  case tryJWT of
    -- If JWT generation failed, log the error and throw a 500
    Left e -> addNamespace "jwt_generation" $ do
      logErrorM [logt|JWT generation failed with the error #{e}|]
      throwM err500

    Right lazyJWT -> pure . JWTText . decodeUtf8 . toStrict $ lazyJWT

-- | Generate a 'UserResponse' with an expiring token (defined in 'Config'),
-- logging to 'Katip' with the given @logAction@ function.
mkUserResponse
  :: HasPassword r UPlainText
  => r -> BCrypt -> User -> App () -> App UserResponse
mkUserResponse user hashedPw dbUser logAction = do
  timeout <- view jwtTimeout
  tok <- mkToken (fromUPlainText $ user ^. password) hashedPw dbUser
  jwt <- mkJWT tok timeout
  logAction
  pure $ UserResponse
    (userEmail dbUser) jwt (userName dbUser) (userBio dbUser) (userImage dbUser)
