module Model.Persist where

-- Prelude.
import           ClassyPrelude

import           Data.UUID            (UUID)
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       persistLowerCase, share, sqlSettings)

-- Local imports.
import           Types.BCrypt         (BCrypt)
import           Types.Instances      ()
import           Types.User           (UName, UEmail, UBio, UImage)

--------------------------------------------------------------------------------
-- | Persistent model definition.
share
  [mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
  User sql=users
    name      UName        sqltype=text
    email     UEmail       sqltype=text
    bio       UBio   Maybe sqltype=text
    image     UImage Maybe sqltype=text
    uuid      UUID         sqltype=uuid
    createdAt UTCTime      sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt UTCTime      sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueEmailUser email
    deriving Eq Generic Show

  Password sql=passwords
    hash      BCrypt
    user      UserId
    createdAt UTCTime sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt UTCTime sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniquePasswordUser user
  |]
