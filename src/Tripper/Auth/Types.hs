module Tripper.Auth.Types
( Login (..)
, CurrentUser (..)
, mkCurrentUser
) where

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
  { currentClientId :: ClientId
  , currentUserId   :: UserId
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

mkCurrentUser :: Entity User -> CurrentUser
mkCurrentUser (Entity key val) = CurrentUser
  { currentClientId = userClientId val
  , currentUserId   = key
  }
