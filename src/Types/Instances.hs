{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Instances where

-- Prelude.
import           ClassyPrelude

import           Data.Aeson
import           Data.Aeson.Types      (Value (String))
import qualified Data.ByteString.Char8 as B8
import           Data.UUID             (UUID)
import qualified Data.UUID             as UUID
import           Database.Persist.Sql

--------------------------------------------------------------------------------
-- | Persistent instances for @UUID@.
instance PersistField UUID where
  toPersistValue uuid = PersistDbSpecific . B8.pack . UUID.toString $ uuid
  fromPersistValue (PersistDbSpecific uuidB8) =
    case UUID.fromString $ B8.unpack uuidB8 of
      Just uuid -> Right uuid
      Nothing   -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

--------------------------------------------------------------------------------
-- | Aeson @FromJSON@ and @ToJSON@ instances for @UUID@.
instance FromJSON UUID where
  parseJSON = withText "UUID" $ \uuidStr ->
    case UUID.fromText uuidStr of
      Just uuid -> pure uuid
      Nothing   -> fail "Failed to parse UUID"

instance ToJSON UUID where
  toJSON = String . UUID.toText
