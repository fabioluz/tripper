module Tripper.Feature.User.DB where

import Data.Aeson
import Database.Persist
import RIO
import RIO.Time
import Tripper.Config
import Tripper.Feature.Shared
import Tripper.Feature.User.Types
import Tripper.Models

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

getUsers :: HasPool env => ClientId -> RIO env [Entity User]
getUsers clientId = runDb $ selectList [UserClientId ==. clientId] []

getUserById :: HasPool env => ClientId -> UserId -> RIO env (Maybe (Entity User))
getUserById clientId userId = runDb $ selectFirst
  [ UserClientId ==. clientId
  , UserId ==. userId
  ] []
