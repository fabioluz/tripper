module Tripper.Main where

import Network.Wai
import Network.Wai.Handler.Warp
import RIO
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Models
import Tripper.Server

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
  destroyLogFunc (configLogFunc cfg)
  pure ()

acquireConfig :: IO Config
acquireConfig = do
  port    <- lookupSetting "PORT" 8090
  logFunc <- createLogFunc
  pool    <- createPool $ fst logFunc
  pure Config
    { configPort    = port
    , configPool    = pool
    , configLogFunc = logFunc
    }

-- loggingMiddleware :: LogFunc -> Application -> Application
-- loggingMiddleware logFunc app req res = do
--   runRIO logFunc
--     $ logDebug 
--     $ mconcat 
--     [ displayShow $ httpVersion req
--     , " "
--     , displayShow $ requestMethod req
--     , " "
--     , displayShow $ fromMaybe "" $ requestHeaderHost req
--     , displayShow $ rawPathInfo req
--     , displayShow $ rawQueryString req
--     ]
--   app req res