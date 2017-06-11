module Api where

import           Servant
import           Servant.Auth.Server

import           Foundation
import           Api.Users

--------------------------------------------------------------------------------
-- | Servant type-level representation of the api.
type Api auths = "api" :> "v1" :> (UsersApi auths)

-- | Servant term-level representation of the api.
api :: Proxy (Api '[JWT])
api = Proxy

-- | Server function for the entire Api.
server :: ServerT (Api auths) App
server = usersServer
