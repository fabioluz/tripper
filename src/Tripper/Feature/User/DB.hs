module Tripper.Feature.User.DB where

import Database.Persist
import RIO
import RIO.Time
import Tripper.Config
import Tripper.Feature.Shared
import Tripper.Feature.User.Types
import Tripper.Models

mkUser :: ClientId -> ValidCreateUser -> AppM env User
mkUser clientId ValidCreateUser {..} = do
  now      <- getCurrentTime
  password <- mkPassword validUserPassword
  pure User
    { userClientId  = clientId
    , userPassword  = password
    , userEmail     = validUserEmail
    , userName      = validUserName
    , userNickName  = validUserNickName
    , userCreatedAt = now
    , userUpdatedAt = now
    }

getUserByEmail :: HasPool env => Text -> AppM env (Maybe (Entity User))
getUserByEmail = runDb . getBy . UniqueUserEmail . Email

getUsers :: (HasPool env, HasLogFunc env) => ClientId -> AppM env [Entity User]
getUsers clientId = do
  logInfo "getting all users"
  runDb $ selectList [UserClientId ==. clientId] []

getUserById :: HasPool env => ClientId -> UserId -> AppM env (Maybe (Entity User))
getUserById clientId userId = runDb $ selectFirst
  [UserClientId ==. clientId, UserId ==. userId]
  []

insertUser :: HasPool env => ClientId -> ValidCreateUser -> AppM env UserId
insertUser clientId fields = do
  user <- mkUser clientId fields
  runDb $ insert user

updateUser :: HasPool env => ClientId -> UserId -> ValidUpdateUser -> AppM env ()
updateUser clientId userId ValidUpdateUser {..} = do
  now <- getCurrentTime
  runDb $ updateWhere
    [ UserClientId ==. clientId, UserId ==. userId ]
    [ UserName =. validUserName
    , UserNickName =. validUserNickName
    , UserUpdatedAt =. now
    ]
