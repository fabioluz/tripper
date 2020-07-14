module Tripper.Server (app) where

import Control.Monad.Except
import RIO hiding (Handler)
import Servant
import Servant.Auth.Server
import Tripper.Feature.Auth.Types
import Tripper.Feature.Auth.Server
import Tripper.Feature.Client.Server 
import Tripper.Feature.User.Server
import Tripper.Config

type JWTAuth = Auth '[JWT] CurrentUser

type AppContext = '[CookieSettings, JWTSettings]

type AppAPI
    =  AuthAPI
  :<|> ClientAPI
  :<|> JWTAuth :> UserAPI


proxyContext :: Proxy AppContext
proxyContext = Proxy

proxyAPI :: Proxy AppAPI
proxyAPI = Proxy

convertApp :: Config -> RIO Config a -> Handler a
convertApp cfg = Handler . ExceptT . try . runRIO cfg

configServer :: CookieSettings -> JWTSettings -> ServerT AppAPI (RIO Config)
configServer cs jwts
    =  authServer cs jwts
  :<|> clientServer
  :<|> userServer

server :: CookieSettings -> JWTSettings -> Config -> Server AppAPI
server cs jwts cfg = hoistServerWithContext proxyAPI proxyContext
  (convertApp cfg) (configServer cs jwts)

app :: Context AppContext -> CookieSettings -> JWTSettings -> Config -> Application
app ctx cs jwts = serveWithContext proxyAPI ctx . server cs jwts
  