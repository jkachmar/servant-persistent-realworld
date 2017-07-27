module Api.User where

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
import           Types.Token
import           Types.User          hiding (userBio, userImage)

--------------------------------------------------------------------------------
-- | Servant type-level representation of the "user" route fragment.
type UserApi auths = Auth auths Token :> ProtectedApi

-- | Handler function for the "user" route fragment.
userHandler :: ServerT (UserApi auths) App
userHandler = protected

--------------------------------------------------------------------------------
-- | Type-level representation of the endpoints protected by @Auth@.
type ProtectedApi = "user" :> Get '[JSON] UserResponse

-- | Check authentication status and dispatch the request to the appropriate
-- endpoint handler.
protected :: AuthResult Token -> ServerT ProtectedApi App
protected (Authenticated t) = echoUser t
protected _                 = throwM err401

-- | User echo endpoint handler.
echoUser :: Token -> App UserResponse
echoUser tok@(Token uUuid) = do
  -- Get the user associated with this UUID, if they exist.
  maybeUser <- runDB $ getUserByUuid uUuid
  dbUser <- case maybeUser of
    Nothing -> throwM err404
    Just (Entity _ dbUser) -> pure dbUser

  -- FIXME - This refreshes the user's JWT every time this endpoint is accessed.
  -- Is this the intended behavior, or should the JWT keep its old expiry?
  timeout <- view jwtTimeout
  jwt <- mkJWT tok timeout

  addNamespace "echo" $
    logInfoM [logt|"#{dbUser} was echoed back to the user."|]

  pure $ UserResponse
    (userEmail dbUser) jwt (userName dbUser) (userBio dbUser) (userImage dbUser)

--------------------------------------------------------------------------------
-- TODO - make a `Common` module to store duplicated code such as this
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
