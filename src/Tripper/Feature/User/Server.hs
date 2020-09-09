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

userServer :: AuthResult CurrentUser -> ServerT UserAPI (AppM Config)
userServer (Authenticated curUser)
    =  getMeHandler curUser 
  :<|> getHandler curUser
  :<|> getByIdHandler curUser
  :<|> postHandler curUser
  :<|> putHandler curUser

userServer _ = throwAll err401

getHandler :: CurrentUser -> AppM Config [UserOutput]
getHandler CurrentUser {..} = do
  users <- DB.getUsers curClientId
  pure $ UserOutput <$> users

getMeHandler :: CurrentUser -> AppM Config UserOutput
getMeHandler CurrentUser {..} = do
  mayUser <- DB.getUserById curClientId curUserId
  case mayUser of
    Nothing   -> throwIO http404
    Just user -> pure $ UserOutput user

getByIdHandler :: CurrentUser -> UserId -> AppM Config UserOutput
getByIdHandler CurrentUser {..} userId = do
  mayUser <- DB.getUserById curClientId userId
  case mayUser of
    Nothing   -> throwIO http404
    Just user -> pure $ UserOutput user

postHandler :: CurrentUser -> CreateUser -> AppM Config NoContent
postHandler CurrentUser {..} input = do
  userRes <- pure $ createUser input
  case userRes of
    Left err   -> throwIO $ http422 err
    Right user -> do
      _ <- DB.insertUser curClientId user
      pure NoContent

putHandler :: CurrentUser -> UserId -> UpdateUser -> AppM Config NoContent
putHandler CurrentUser {..} userId input = do
  userRes <- pure $ updateUser input
  case userRes of
    Left err   -> throwIO $ http422 err
    Right user -> do
      DB.updateUser curClientId userId user
      pure NoContent