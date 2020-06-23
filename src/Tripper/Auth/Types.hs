module Tripper.Auth.Types where

import RIO
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist (Entity (..))
import Servant.Auth.Server (FromJWT, ToJWT)
import Tripper.Models (ClientId, User (..), UserId)

data Login = Login 
  { email    :: Text
  , password :: Text
  } deriving (Generic, FromJSON)

data CurrentUser = CurrentUser
  { currentClientId :: ClientId
  , currentUserId   :: UserId
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

mkCurrentUser :: Entity User -> CurrentUser
mkCurrentUser (Entity key val) = CurrentUser
  { currentClientId = userClientId val
  , currentUserId   = key
  }
