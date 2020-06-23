module Tripper.Server where

import RIO hiding (Handler)
import Control.Monad.Except (ExceptT (..))
import Servant 
  ( Application
  , Context
  , Handler (..)
  , Proxy (..)
  , Server
  , ServerT
  , serveWithContext
  , hoistServerWithContext
  , (:>)
  , (:<|>) (..)
  )
import Servant.Auth.Server (Auth, AuthResult, CookieSettings, JWTSettings, JWT)
import Tripper.Auth.Server (AuthAPI, authServer)
import Tripper.Auth.Types (CurrentUser)
import Tripper.Config (Config, HasConfig)
import Tripper.Client.Server (ClientAPI, clientServer)

type AppContext = '[CookieSettings, JWTSettings]

proxyContext :: Proxy AppContext
proxyContext = Proxy

type AppAPI = AuthAPI :<|> Auth '[JWT] CurrentUser :> ClientAPI

proxyAPI :: Proxy AppAPI
proxyAPI = Proxy

runApp :: Config -> RIO Config a -> IO a
runApp cfg = flip runReaderT cfg . unRIO

convertApp :: Config -> RIO Config a -> Handler a
convertApp cfg = Handler . ExceptT . try . runApp cfg

server :: CookieSettings -> JWTSettings -> ServerT AppAPI (RIO Config)
server cs jwts = authServer cs jwts :<|> clientServer

hoistServer :: CookieSettings -> JWTSettings -> Config -> Server AppAPI
hoistServer cs jwts cfg = hoistServerWithContext
  proxyAPI proxyContext (convertApp cfg) (server cs jwts)

app :: Context AppContext -> CookieSettings -> JWTSettings -> Config -> Application
app ctx cs jwts = serveWithContext proxyAPI ctx . hoistServer cs jwts