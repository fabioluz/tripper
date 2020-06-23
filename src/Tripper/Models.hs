module Tripper.Models where

import RIO
import RIO.Time (UTCTime)
import Data.UUID (UUID)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )
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