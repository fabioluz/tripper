module Tripper.Feature.Auth.Types where

import Data.Aeson
import Database.Persist
import RIO
import Servant.Auth.Server
import Tripper.Feature.User.Types
import Tripper.Models

-- | Represents the input for login
data Login = Login
  { email    :: Text
  , password :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON

-- | Represents the output for login
data LoginOutput = LoginOutput
  { token :: ByteString 
  , user  :: Entity User 
  }

-- | Represents logged in user 
data CurrentUser = CurrentUser
  { curClientId :: ClientId
  , curUserId   :: UserId
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, FromJWT, ToJWT)

mkCurrentUser :: Entity User -> CurrentUser
mkCurrentUser (Entity userId user) = CurrentUser
  { curClientId = userClientId user
  , curUserId   = userId
  }

instance ToJSON LoginOutput where
  toJSON LoginOutput {..} = object
    [ "token" .= decodeUtf8Lenient token
    , "user"  .= toJSON (UserOutput user)
    ]