 {-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Tripper.Models where

import Database.Persist.Sql
import Database.Persist.TH
import RIO
import RIO.Time
import Tripper.Config
import Tripper.Feature.Shared

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Client json sql=clients
    Id        GUID     default=uuid_generate_v4()
    name      Text
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show Eq

  User sql=users
    Id        GUID      default=uuid_generate_v4()
    clientId  ClientId
    email     Email
    password  Password
    name      Text
    nickName  Text Maybe
    createdAt UTCTime
    updatedAt UTCTime
    UniqueUserEmail email
    deriving Show Eq
|]

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool $ runMigration migrateAll

runDb :: HasPool env => SqlPersistT IO a -> AppM env a
runDb query = do
  pool <- view poolL
  liftIO $ runSqlPool query pool
