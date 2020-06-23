module Tripper.Config where

import RIO
import Database.Persist.Postgresql (ConnectionPool)
import Tripper.DB (HasPool (..))

data Config = Config
  { configPort :: Int
  , configPool :: ConnectionPool
  }

class HasPool env => HasConfig env where
  configL :: Lens' env Config

instance HasPool Config where
  poolL = lens configPool $ \cfg pool -> cfg { configPool = pool }

instance HasConfig Config where
  configL = id

