module Tripper.Feature.User.Server (UserAPI, userServer) where

import RIO
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Models
import Tripper.Feature.Auth.Types
import Tripper.Feature.User.DB
import Tripper.Feature.User.Types

type UserAPI
    = "me"
      :> Get '[JSON] UserOutput
  :<|> "users"
      :> Get '[JSON] [UserOutput]
  :<|> "users"
      :> Capture "userId" UserId 
      :> Get '[JSON] UserOutput

userServer :: HasConfig env => AuthResult CurrentUser -> ServerT UserAPI (RIO env)
userServer (Authenticated curUser)
    =  getMeHandler curUser 
  :<|> getHandler curUser
  :<|> getByIdHandler curUser

userServer _ = throwAll err401

getHandler :: HasConfig env => CurrentUser -> RIO env [UserOutput]
getHandler CurrentUser {..} = do
  users <- getUsers curClientId
  pure $ UserOutput <$> users

getMeHandler :: HasConfig env => CurrentUser -> RIO env UserOutput
getMeHandler CurrentUser {..} = do
  mayUser <- getUserById curClientId curUserId
  case mayUser of
    Nothing   -> throwIO err404
    Just user -> pure $ UserOutput user

getByIdHandler :: HasConfig env => CurrentUser -> UserId -> RIO env UserOutput
getByIdHandler CurrentUser {..} userId = do
  mayUser <- getUserById curClientId userId
  case mayUser of
    Nothing   -> throwIO err404
    Just user -> pure $ UserOutput user