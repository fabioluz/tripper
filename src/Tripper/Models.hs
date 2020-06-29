module Tripper.Models where

import Data.UUID
import Database.Persist.Sql
import Database.Persist.TH
import RIO
import RIO.Time
import Tripper.Config
import Tripper.Feature.Shared

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Client json sql=clients
    Id        UUID     default=uuid_generate_v4()
    name      Text
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show Eq

  User json sql=users
    Id        UUID      default=uuid_generate_v4()
    clientId  ClientId
    email     Email
    password  Password
    name      Text
    createdAt UTCTime
    updatedAt UTCTime
    UniqueUserEmail email
    deriving Show Eq
|]

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool $ runMigration migrateAll

runDb :: HasPool env => SqlPersistT IO a -> RIO env a
runDb query = do
  pool <- view poolL
  liftIO $ runSqlPool query pool
