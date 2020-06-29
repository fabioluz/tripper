module Tripper.Main where

import RIO
import Network.Wai.Handler.Warp (run)
import Servant (Context (..))
import Servant.Auth.Server (JWTSettings (..), defaultJWTSettings, defaultCookieSettings, generateKey)
import Tripper.Config
import Tripper.Models
import Tripper.Server (app)

main :: IO ()
main = bracket acquireConfig shutdownApp startApp

startApp :: Config -> IO ()
startApp env = do
  key <- generateKey
  let jwt     = defaultJWTSettings key
      cookie  = defaultCookieSettings
      context = cookie :. jwt :. EmptyContext
      pool    = configPool env
      port    = configPort env
  runMigrations pool
  run port $ app context cookie jwt env

shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  destroyPool (configPool cfg)
  pure () 

acquireConfig :: IO Config
acquireConfig = do
  port <- lookupSetting "PORT" 8090
  pool <- createPool
  pure Config
    { configPort = port
    , configPool = pool
    }

