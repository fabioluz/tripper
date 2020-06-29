module Tripper.Config where

import RIO
import Control.Monad.Logger
import Data.Pool 
import Database.Persist.Postgresql
import System.Environment
import Servant.Auth.Server


data Config = Config
  { configPort :: Int
  , configPool :: ConnectionPool
  }


-- |
-- | Database Configuration
-- |

class HasPool env where
  poolL :: Lens' env ConnectionPool

instance HasPool Config where
  poolL = lens configPool $ \cfg pool -> cfg { configPool = pool }

defaultConnStr :: ConnectionString 
defaultConnStr = "host=127.0.0.1 port=5432 user=postgres password=102030 dbname=ride connect_timeout=10"

createPool :: IO ConnectionPool 
createPool = do
  connStr <- lookupSetting "CONN_STR" defaultConnStr
  runStdoutLoggingT $ createPostgresqlPool connStr 10

destroyPool :: ConnectionPool -> IO ()
destroyPool = destroyAllResources

-- |
-- | Utilities
-- |

class HasPool env => HasConfig env where
  configL :: Lens' env Config

instance HasConfig Config where
  configL = id

instance ThrowAll (RIO env a) where
  throwAll = throwIO

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  var <- lookupEnv env
  pure $ fromMaybe def $ var >>= readMaybe
  