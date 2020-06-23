module Tripper.Server (app) where

import Control.Monad.Except
import RIO hiding (Handler)
import Servant
import Servant.Auth.Server
import Tripper.Auth
import Tripper.Client.Server 
import Tripper.Config

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

configServer :: CookieSettings -> JWTSettings -> ServerT AppAPI (RIO Config)
configServer cs jwts = authServer cs jwts :<|> clientServer

server :: CookieSettings -> JWTSettings -> Config -> Server AppAPI
server cs jwts cfg = hoistServerWithContext proxyAPI proxyContext
  (convertApp cfg) (configServer cs jwts)

app :: Context AppContext -> CookieSettings -> JWTSettings -> Config -> Application
app ctx cs jwts = serveWithContext proxyAPI ctx . server cs jwts
