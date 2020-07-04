module Tripper.Feature.User.Types where

import Data.Aeson
import RIO
import RIO.Time
import Tripper.Feature.Client.Types (CreateClient(..))
import Tripper.Feature.Shared
import Tripper.Models

-- |
-- | Inputs
-- |

data CreateUser = CreateUser
  { email    :: Text
  , password :: Text
  , name     :: Text
  , nickName :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ValidCreateUser = ValidCreateUser
  { validUserEmail    :: Email
  , validUserPassword :: Text
  , validUserName     :: Text
  , validUserNickName :: Maybe Text
  }

createUser :: CreateUser -> Either ValidationErrors ValidCreateUser
createUser CreateUser {..} = ValidCreateUser
  <$> validateEmail email
  <*> validatePassword password
  <*> validateName name
  <*> validateNickName nickName

newtype UpdateUser = UpdateUser { name :: Text }
  deriving (Show, Generic)
  deriving newtype FromJSON

newtype ValidUpdateUser = ValidUpdateUser { name :: Text }

updateUser :: UpdateUser -> Either ValidationErrors ValidUpdateUser
updateUser UpdateUser {..} = ValidUpdateUser <$> validateName name

-- |
-- | Validatation
-- |

validateEmail :: Text -> Either ValidationErrors Email
validateEmail = mapValError "email" . mkEmail

validatePassword :: Text -> Either ValidationErrors Text
validatePassword = mapValError "password" . validateText [minLength 6]

validateName :: Text -> Either ValidationErrors Text
validateName = mapValError "name" . validateText [notEmpty]

validateNickName :: Maybe Text -> Either ValidationErrors (Maybe Text)
validateNickName = mapM $ mapValError "nickName" . validateText [notEmpty]

fromCreateClient :: CreateClient -> CreateUser
fromCreateClient CreateClient {..} = CreateUser
  { email    = adminEmail
  , password = adminPassword
  , name     = adminName
  , nickName = Nothing
  }

-- |
-- | Outputs
-- |

newtype UserOutput = UserOutput (Entity User)

instance ToJSON UserOutput where
  toJSON userOutput = object
    [ "userId"    .= userId
    , "clientId"  .= userClientId
    , "email"     .= userEmail
    , "name"      .= userName
    , "nickName"  .= userNickName
    , "createdAt" .= userCreatedAt
    , "updatedAt" .= userUpdatedAt
    ]
    where
      UserOutput entity  = userOutput
      Entity userId user = entity
      User {..}          = user



