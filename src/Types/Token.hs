module Types.Token where

-- Prelude.
import           ClassyPrelude

import           Data.Aeson           (FromJSON, ToJSON, object, parseJSON,
                                       toJSON, withObject, (.:), (.=))
import           Data.UUID            (UUID, fromText, toText)
import           Servant.Auth.Server  (FromJWT, JWTSettings, ToJWT, encodeJWT,
                                       key)

-- Imports to override 'makeJWT'
import           Control.Lens         ((&), (.~))
import           Control.Monad.Except (runExceptT)
import qualified Crypto.JOSE          as Jose
import qualified Crypto.JWT           as Jose
import qualified Data.ByteString.Lazy as BSL

-------------------------------------------------------------------------------
-- | For now, the @Token@ is simply a wrapper around a @UUID@, with the
-- necessary instances to generate a @JWT@ or parse the @UUID@ back out from it.
newtype Token = Token { unToken :: UUID } deriving (Eq, Read, Show)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \o -> do
     maybeTok <- o .: "user_uuid"
     case (fromText maybeTok) of
       Nothing  -> fail $ "unable to parse field [" <> unpack maybeTok <> "]"
       Just tok -> pure . Token $ tok

instance ToJSON Token where
  toJSON (Token tok) = object [ "user_uuid" .= (toText tok) ]

instance FromJWT Token
instance ToJWT   Token

-------------------------------------------------------------------------------
-- | Simple newtype wrapper for the textual representation of a JWT
newtype JWTText = JWTText Text deriving (Generic, ToJSON)

-------------------------------------------------------------------------------
-- | Creates a JWT containing the specified data. The data is stored in the
-- @dat@ claim. The 'Maybe UTCTime' argument indicates the time at which the
-- token expires. Uses @RS256@ rather than 'servant-auth's standard @HS256@.
makeJWT
  :: ToJWT a
  => a -> JWTSettings -> Maybe UTCTime
  -> IO (Either Jose.Error BSL.ByteString)
makeJWT v cfg expiry = runExceptT $ do
  ejwt <- Jose.createJWSJWT (key cfg)
                            (Jose.newJWSHeader (Jose.Protected, Jose.RS256))
                            (addExp $ encodeJWT v)

  Jose.encodeCompact ejwt
  where
   addExp claims = case expiry of
     Nothing -> claims
     Just e  -> claims & Jose.claimExp .~ Just (Jose.NumericDate e)
