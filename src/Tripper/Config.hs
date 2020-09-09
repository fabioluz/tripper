module Tripper.Config where

import Data.Pool
import Database.Persist.Postgresql hiding (LogFunc)
import RIO
import RIO.Orphans ()
import Servant.Auth.Server hiding (def)
import System.Environment

-- |
-- | App Monad
-- |

newtype AppM env a = AppM { unAppM :: RIO env a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader env
    , MonadThrow
    , MonadUnliftIO
    , Semigroup
    )

instance ThrowAll (AppM env a) where
  throwAll = throwIO

liftAppM :: (MonadIO m, MonadReader env m) => AppM env a -> m a 
liftAppM = liftRIO . unAppM

runAppM :: MonadIO m => env -> AppM env a -> m a 
runAppM env = runRIO env . unAppM

-- |
-- | Application Environment
-- | 

data Config = Config
  { configPort    :: Int
  , configEnv     :: Environment
  , configPool    :: ConnectionPool
  , configLogFunc :: (LogFunc, IO ()) -- (logger, dispose function)
  }

data Environment = Development | Production
  deriving (Eq, Read)

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

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  var <- lookupEnv env
  pure $ fromMaybe def $ var >>= readMaybe
