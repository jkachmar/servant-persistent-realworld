module Api.Users where

-- * Prelude.
import           ClassyPrelude hiding (hash)

-- * Base imports.
import           Control.Lens
import           Data.Time           (NominalDiffTime, addUTCTime)
-- import           Database.Persist    (Entity(Entity))
import           Katip               (logTM, Severity(..), LogStr, logStr, Namespace)

-- * Servant imports.
import           Servant
import           Servant.Auth.Server hiding (makeJWT)

-- * Local imports.
import           Foundation
import           Logging
import           Model
import           Query.User
import           Types.BCrypt
import           Types.Token
import           Types.User          hiding (userBio, userImage)

--------------------------------------------------------------------------------
-- | Servant type-level representation of the "users" route fragment.
type UsersApi auths = (Auth auths Token :> ProtectedApi) -- :<|> UnprotectedApi

-- | Server function for the "users" route fragment.
usersServer :: ServerT (UsersApi auths) App
usersServer = protected -- :<|> unprotected

--------------------------------------------------------------------------------
-- | Type-level representation of the endpoints protected by @Auth@.
type ProtectedApi =
       "users"
         :> "register"
           :> ReqBody '[JSON] UserRegister
             :> Post '[JSON] UserResponse

-- | Check authentication status and dispatch to the appropriate handler.
protected :: AuthResult Token -> ServerT ProtectedApi App
protected (Authenticated t) = register t
protected _                 = throwAll err401

-- | Registration endpoint handler.
register :: Token -> UserRegister -> App UserResponse
register _ userReg = do
  hashedPw <- hashPassword $ fromUPlainText $ userReg^.password
  dbUser   <- runDB $ insertUser (userReg^.name) (userReg^.email) hashedPw

  let logMsg = "Registered: [["
               <> (logStr . tshow $ dbUser)
               <> "]]"

  mkUserResponse userReg hashedPw dbUser ("register", logMsg)

--------------------------------------------------------------------------------
-- | Return a token for a given user if the login password is valid when
-- compared to the hash in the database; throw 401 if the user's password
-- is invalid
mkToken :: Text -> BCrypt -> User -> App Token
mkToken pass hashed dbUser = do
  -- Validate the stored hash against the plaintext password
  isValid <- validatePassword pass hashed

  -- If the password isn't valid, throw a 401
  if isValid then pure () else throwError err401
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
    Left e -> do
      $(logTM) ErrorS
        $ "JWT generation failed with the following error [["
        <> (logStr . tshow $ e)
        <> "]]"
      throwError err500

    Right lazyJWT -> pure . JWTText . decodeUtf8 . toStrict $ lazyJWT

-- | Generate a @UserResponse@ with a token that expires 20 minutes from now.
mkUserResponse
  :: HasPassword s UPlainText
  => s -> BCrypt -> User -> (Namespace, LogStr) -> App UserResponse
mkUserResponse user hashedPw dbUser (logNamespace, logMsg) = do
  tok <- mkToken (fromUPlainText $ user^.password) hashedPw dbUser
  jwt <- mkJWT tok 1200
  addNamespace logNamespace $ $(logTM) InfoS logMsg
  pure $ UserResponse
    (userEmail dbUser) jwt (userName dbUser) (userBio dbUser) (userImage dbUser)
