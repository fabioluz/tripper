module Tripper.DB where

import RIO
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (destroyAllResources)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool, runSqlPool)
import Database.Persist.Sql (Key, SqlBackend, SqlPersistT, ToBackendKey)

class HasPool env where
  poolL :: Lens' env ConnectionPool

connStr :: ConnectionString 
connStr = "host=127.0.0.1 port=5432 user=postgres password=102030 dbname=ride connect_timeout=10"

createPool :: IO ConnectionPool 
createPool = runStdoutLoggingT $ createPostgresqlPool connStr 10

destroyPool :: ConnectionPool -> IO ()
destroyPool = destroyAllResources

runDb :: HasPool env => SqlPersistT IO a -> RIO env a
runDb query = do
  pool <- view poolL
  liftIO $ runSqlPool query pool