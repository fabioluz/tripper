module Tripper.Main where

import Network.Wai.Handler.Warp
import RIO
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Middlewares
import Tripper.Models
import Tripper.Server

main :: IO ()
main = bracket acquireConfig shutdownApp startApp

startApp :: Config -> IO ()
startApp env = do
  key <- generateKey
  let cs      = defaultCookieSettings
      jwts    = defaultJWTSettings key
      context = cs :. jwts :. EmptyContext
      pool    = configPool env
      port    = configPort env
      logger  = fst $ configLogFunc env
  runMigrations pool
  run port
    $ addMiddlewares env
    $ app context jwts env

shutdownApp :: Config -> IO ()
shutdownApp Config {..} = do
  destroyPool configPool
  destroyLogFunc configLogFunc
  pure ()

acquireConfig :: IO Config
acquireConfig = do
  port    <- lookupSetting "PORT" 8090
  env     <- lookupSetting "ENV" Development
  logFunc <- createLogFunc
  pool    <- createPool $ fst logFunc
  pure Config
    { configPort    = port
    , configEnv     = env
    , configPool    = pool
    , configLogFunc = logFunc
    }
