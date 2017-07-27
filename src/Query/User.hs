module Query.User where

-- Prelude.
import           ClassyPrelude      hiding (on)

import           Data.UUID          (UUID)
import           Data.UUID.V4       (nextRandom)
import           Database.Esqueleto

-- Local imports.
import           Model
import           Types.BCrypt
import           Types.User

--------------------------------------------------------------------------------
-- | Insert a new user into the database.
insertUser :: UName -> UEmail -> BCrypt -> DB User
insertUser uName uEmail uPass = do
  now     <- liftIO $ getCurrentTime
  newUuid <- liftIO $ nextRandom

  (Entity userKey userRec) <- insertEntity $
    User uName uEmail Nothing Nothing newUuid now now

  _ <- insert $ Password uPass userKey now now

  pure $ userRec

--------------------------------------------------------------------------------
-- | Retrieve a user and their hashed password from the database.
getUserByEmail :: UEmail -> DB (Maybe (Entity User, Entity Password))
getUserByEmail uEmail = fmap listToMaybe $
  select $
  from $ \(dbUser `InnerJoin` dbPass) -> do
  on (dbUser ^. UserId ==. dbPass ^. PasswordUser)
  where_ (dbUser ^. UserEmail ==. val uEmail)
  pure (dbUser, dbPass)

-- | Retrieve a user and their hashed password from the database.
getUserByUuid :: UUID -> DB (Maybe (Entity User))
getUserByUuid uUuid = fmap listToMaybe $
  select $
  from $ \dbUser -> do
  where_ (dbUser ^. UserUuid ==. val uUuid)
  pure dbUser
