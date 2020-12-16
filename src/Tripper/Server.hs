module Tripper.Server (app) where

import Control.Monad.Except
import RIO hiding (Handler)
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Feature.Auth.Types
import Tripper.Feature.Auth.Server
import Tripper.Feature.Client.Server 
import Tripper.Feature.Translation.Server
import Tripper.Feature.User.Server

type JWTAuth = Auth '[JWT] CurrentUser

type AppContext = '[CookieSettings, JWTSettings] -- Why we have to pass cookie settings?

type AppAPI
    =  AuthAPI
  :<|> TranslationAPI
  :<|> ClientAPI
  :<|> JWTAuth :> UserAPI

proxyContext :: Proxy AppContext
proxyContext = Proxy

proxyAPI :: Proxy AppAPI
proxyAPI = Proxy

convertApp :: Config -> AppM Config a -> Handler a
convertApp cfg = Handler . ExceptT . try . runAppM cfg

configServer :: JWTSettings -> ServerT AppAPI (AppM Config)
configServer jwts
    =  authServer jwts
  :<|> translationServer
  :<|> clientServer
  :<|> userServer

server :: JWTSettings -> Config -> Server AppAPI
server jwts cfg = hoistServerWithContext proxyAPI proxyContext
  (convertApp cfg) (configServer jwts)

app :: Context AppContext -> JWTSettings -> Config -> Application
app ctx jwts = serveWithContext proxyAPI ctx . server jwts
  