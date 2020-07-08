module Tripper.Config where

import Data.Pool
import Database.Persist.Postgresql hiding (LogFunc)
import RIO
import RIO.Orphans
import Servant.Auth.Server
import System.Environment

data Config = Config
  { configPort    :: Int
  , configPool    :: ConnectionPool
  , configLogFunc :: (LogFunc, IO ()) -- (logger, dispose function)
  }

-- |
-- | Logging
-- |

createLogFunc :: IO (LogFunc, IO ())
createLogFunc = do
  logOptions <- logOptionsHandle stdout True
  newLogFunc logOptions

destroyLogFunc :: (LogFunc, IO ()) -> IO ()
destroyLogFunc = snd

instance HasLogFunc Config where
  logFuncL = lens (fst . configLogFunc) const

-- |
-- | Database Configuration
-- |

class HasPool env where
  poolL :: Lens' env ConnectionPool

instance HasPool Config where
  poolL = lens configPool const

defaultConnStr :: ConnectionString
defaultConnStr = "host=127.0.0.1 port=5432 user=postgres password=102030 dbname=ride connect_timeout=10"

createPool :: LogFunc -> IO ConnectionPool
createPool logFunc = runRIO logFunc $ do
  connStr <- liftIO $ lookupSetting "CONN_STR" defaultConnStr
  createPostgresqlPool connStr 10

destroyPool :: ConnectionPool -> IO ()
destroyPool = destroyAllResources

-- |
-- | Utilities
-- |

instance ThrowAll (RIO env a) where
  throwAll = throwIO

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  var <- lookupEnv env
  pure $ fromMaybe def $ var >>= readMaybe

-- |
-- | Final Configuration
-- | 

class (HasPool env, HasLogFunc env) => HasConfig env where
  configL :: Lens' env Config

instance HasConfig Config where
  configL = id
