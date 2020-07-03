module Tripper.Feature.User.Server (UserAPI, userServer) where

import RIO
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Models
import Tripper.Feature.Shared
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
  :<|> "users"
      :> ReqBody '[JSON] CreateUser
      :> PostCreated '[JSON] NoContent

userServer :: HasConfig env => AuthResult CurrentUser -> ServerT UserAPI (RIO env)
userServer (Authenticated curUser)
    =  getMeHandler curUser 
  :<|> getHandler curUser
  :<|> getByIdHandler curUser
  :<|> postHandler curUser

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

postHandler :: HasConfig env => CurrentUser -> CreateUser -> RIO env NoContent
postHandler CurrentUser {..} input = do
  user <- createUser input `orThrow` http422
  insertUser curClientId user
  pure NoContent

putHandler :: HasConfig env => CurrentUser -> UpdateUser -> RIO env NoContent
putHandler CurrentUser {..} input = do
  user <- updateUser input `orThrow` http422
  updateUser curClientId user
  pure NoContent