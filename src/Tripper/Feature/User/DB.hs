module Tripper.Feature.User.DB where

import RIO
import RIO.Time 
import Database.Persist
import Tripper.DB
import Tripper.Models
import Tripper.Feature.Shared
import Tripper.Feature.User.Types

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
