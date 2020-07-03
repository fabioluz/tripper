module Tripper.Feature.Auth.Types where

import Data.Aeson
import Database.Persist
import RIO
import Servant.Auth.Server
import Tripper.Models

data Login = Login
  { email    :: Text
  , password :: Text
  } deriving (Generic, FromJSON)

data CurrentUser = CurrentUser
  { curClientId :: ClientId
  , curUserId   :: UserId
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

mkCurrentUser :: Entity User -> CurrentUser
mkCurrentUser (Entity key val) = CurrentUser
  { curClientId = userClientId val
  , curUserId   = key
  }