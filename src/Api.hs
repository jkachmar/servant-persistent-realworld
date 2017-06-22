module Api where

-- Servant imports.
import           Servant
import           Servant.Auth.Server

-- Local project imports.
import           Foundation
import           Api.User
import           Api.Users

--------------------------------------------------------------------------------
-- | Servant type-level reprsentation of all routes requiring authentication.
type Api' auths = (UserApi auths) :<|> (UsersApi auths)

-- | Servant type-level representation of the top-level api.
type Api auths = "api" :> "v1" :> Api' auths

-- | Servant term-level representation of the api.
api :: Proxy (Api '[JWT])
api = Proxy

-- | Handler function for the entire Api.
handler :: ServerT (Api auths) App
handler = userHandler :<|> usersHandler
