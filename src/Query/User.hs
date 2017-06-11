module Query.User where

import           ClassyPrelude      hiding (on)
import           Data.UUID.V4       (nextRandom)
import           Database.Esqueleto

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
getUser :: UEmail -> DB (Maybe (Entity User, Entity Password))
getUser uEmail = fmap listToMaybe $
  select $
  from $ \(dbUser `InnerJoin` dbPass) -> do
  on (dbUser ^. UserId ==. dbPass ^. PasswordUser)
  where_ (dbUser ^. UserEmail ==. val uEmail)
  pure (dbUser, dbPass)
