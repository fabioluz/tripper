module Tripper.Feature.User.Types where

import RIO
import RIO.Time 
import Data.Aeson 
import Tripper.Models
import Tripper.Feature.Client.Types (CreateClient (..))
import Tripper.Feature.Shared

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

