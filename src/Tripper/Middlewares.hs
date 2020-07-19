module Tripper.Middlewares (addMiddlewares) where

import Network.HTTP.Types
import Network.Wai
import RIO
import Tripper.Config

-- |
-- | Logging
-- | 

logStartRequest :: Request -> RIO LogFunc () 
logStartRequest req = logDebug $ mconcat
  [ "REQUEST - "
  , displayShow $ httpVersion req
  , " "
  , displayBytesUtf8 $ requestMethod req
  , " "
  , displayBytesUtf8 $ fromMaybe "" (requestHeaderHost req)
  , displayBytesUtf8 $ rawPathInfo req
  , displayBytesUtf8 $ rawQueryString req
  ]

logEndRequest :: Response -> RIO LogFunc ()
logEndRequest res = logDebug $ mconcat
  [ "RESPONSE - STATUS "
  , displayShow $ statusCode status
  , " "
  , displayBytesUtf8 $ statusMessage status
  ]
  where
    status = responseStatus res

loggingMiddleware :: LogFunc -> Middleware
loggingMiddleware logFunc app req sendRes = do
  runRIO logFunc (logStartRequest req)
  app req \res -> do
    runRIO logFunc (logEndRequest res)
    sendRes res

middlewares :: Config -> [Middleware]
middlewares Config {..} = case configEnv of
  Development -> [loggingMiddleware logFunc]
  Production  -> []
  where
    logFunc = fst configLogFunc

addMiddlewares :: Config -> Application -> Application
addMiddlewares env app = foldr ($) app (middlewares env)