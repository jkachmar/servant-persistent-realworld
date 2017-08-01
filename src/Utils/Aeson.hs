module Utils.Aeson where

-- Prelude.
import           ClassyPrelude

import           Data.Aeson
import           Data.Aeson.Types     (Parser, Options(..), camelTo2)
import           Data.Typeable        (Proxy(..), typeRep)
import           GHC.Generics         (Rep)

-------------------------------------------------------------------------------
-- | Aeson deserializer to generically format the given record based on its
-- type information.
gCustomParseJSON
  :: forall a
   . ( GFromJSON Zero (Rep a)
     , Generic a
     , Typeable a)
  => (Proxy a -> Options)
  -> Value -> Parser a
gCustomParseJSON fmt = genericParseJSON . fmt $ Proxy @a

-- | Aeson serializer to generically format the given record based on its type
-- information.
gCustomToJSON
  :: forall a
   . ( GToJSON Zero (Rep a)
     , Generic a
     , Typeable a)
  => (Proxy a -> Options)
  -> a -> Value
gCustomToJSON fmt = genericToJSON . fmt $ Proxy @a

-- | Modify given Aeson options to drop the type name from some record's field
-- labels, and "snake_case"-ing.
gSnakeCase :: forall proxy a. Typeable a => Options -> proxy a -> Options
gSnakeCase opts _ = opts { fieldLabelModifier = gTrim }
  where gTrim :: String -> String
        gTrim = camelTo2 '_' . (drop . length . show $ typeRep $ Proxy @a)

--------------------------------------------------------------------------------
-- | Unwrap the top-level object of some JSON before deserializing it.
unwrapJson
  :: forall a b
   . (FromJSON a, Typeable b)
  => Text -> (a -> Parser b) -> Value -> Parser b
unwrapJson field parser =
  let label = show . typeRep $ Proxy @b
  in withObject label $ \o -> do
    u <- o .: field
    parser u

-- | Wrap the data type to-be-serialized in some top-level object.
wrapJson :: ToJSON a => Text -> a -> Value
wrapJson label nested = object [ label .= nested ]
