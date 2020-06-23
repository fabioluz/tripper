module Tripper.User.Types where

import RIO
import RIO.Time (getCurrentTime)
import Data.Aeson (FromJSON, ToJSON)
import Tripper.Models (ClientId, User (..))
import Tripper.Client.Types (CreateClient (..))
import Tripper.Shared.Types (Email, mkEmail, mkPassword)
import Tripper.Shared.Validators.Error (ValidationErrors, mapValError)
import Tripper.Shared.Validators.Text (validateText, notEmpty, minLength)

data CreateUser = CreateUser
  { email    :: Text
  , password :: Text
  , name     :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data ValidCreateUser = ValidCreateUser
  { validUserEmail    :: Email
  , validUserPassword :: Text
  , validUserName     :: Text
  }

createUser :: CreateUser -> Either ValidationErrors ValidCreateUser
createUser CreateUser {..} =
  ValidCreateUser
  <$> validateEmail email
  <*> validatePassword password
  <*> validateName name

validateEmail :: Text -> Either ValidationErrors Email
validateEmail = mapValError "email" . mkEmail

validatePassword :: Text -> Either ValidationErrors Text
validatePassword = mapValError "password" . validateText [minLength 6]

validateName :: Text -> Either ValidationErrors Text
validateName = mapValError "name" . validateText [notEmpty]

fromCreateClient :: CreateClient -> CreateUser
fromCreateClient CreateClient {..} = CreateUser
  { email    = adminEmail
  , password = adminPassword
  , name     = adminName
  }

