module Tripper.Feature.User.Server (UserAPI, userServer) where

import RIO
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Models
import Tripper.Feature.Shared
import Tripper.Feature.Auth.Types
import Tripper.Feature.User.Types

import qualified Tripper.Feature.User.DB as DB

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
  :<|> "users"
      :> Capture "userId" UserId 
      :> ReqBody '[JSON] UpdateUser
      :> Put '[JSON] NoContent

userServer :: HasConfig env => AuthResult CurrentUser -> ServerT UserAPI (RIO env)
userServer (Authenticated curUser)
    =  getMeHandler curUser 
  :<|> getHandler curUser
  :<|> getByIdHandler curUser
  :<|> postHandler curUser
  :<|> putHandler curUser

userServer _ = throwAll err401

getHandler :: HasConfig env => CurrentUser -> RIO env [UserOutput]
getHandler CurrentUser {..} = do
  users <- DB.getUsers curClientId
  pure $ UserOutput <$> users

getMeHandler :: HasConfig env => CurrentUser -> RIO env UserOutput
getMeHandler CurrentUser {..} = do
  mayUser <- DB.getUserById curClientId curUserId
  case mayUser of
    Nothing   -> throwIO http404
    Just user -> pure $ UserOutput user

getByIdHandler :: HasConfig env => CurrentUser -> UserId -> RIO env UserOutput
getByIdHandler CurrentUser {..} userId = do
  mayUser <- DB.getUserById curClientId userId
  case mayUser of
    Nothing   -> throwIO http404
    Just user -> pure $ UserOutput user

postHandler :: HasConfig env => CurrentUser -> CreateUser -> RIO env NoContent
postHandler CurrentUser {..} input = do
  userRes <- toRIO $ createUser input
  case userRes of
    Left error -> throwIO $ http422 error
    Right user -> do
      DB.insertUser curClientId user
      pure NoContent

putHandler :: HasConfig env => CurrentUser -> UserId -> UpdateUser -> RIO env NoContent
putHandler CurrentUser {..} userId input = do
  userRes <- toRIO $ updateUser input
  case userRes of
    Left error -> throwIO $ http422 error
    Right user -> do
      DB.updateUser curClientId userId user
      pure NoContent