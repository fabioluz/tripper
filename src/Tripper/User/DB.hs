module Tripper.User.DB where

import RIO
import RIO.Time (getCurrentTime)
import Database.Persist (Entity, getBy)
import Tripper.DB (HasPool, runDb)
import Tripper.Models (ClientId, User (..), Unique (..))
import Tripper.Feature.Shared
import Tripper.User.Types (ValidCreateUser (..))

mkUser :: ClientId -> ValidCreateUser -> RIO env User
mkUser clientId ValidCreateUser {..} = do
  now      <- getCurrentTime
  password <- mkPassword validUserPassword
  pure User
    { userClientId  = clientId
    , userPassword  = password
    , userEmail     = validUserEmail
    , userName      = validUserName
    , userCreatedAt = now
    , userUpdatedAt = now
    }

getUserByEmail :: HasPool env => Text -> RIO env (Maybe (Entity User))
getUserByEmail = runDb . getBy . UniqueUserEmail . Email
