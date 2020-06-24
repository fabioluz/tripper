module Tripper.Feature.Client.Types where

import Data.Aeson
import RIO
import Tripper.Feature.Shared

data CreateClient = CreateClient
  { clientName    :: Text
  , adminEmail    :: Text
  , adminPassword :: Text
  , adminName     :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ValidCreateClient = ValidCreateClient
  { validClientName :: Text
  }

createClient :: CreateClient -> Either ValidationErrors ValidCreateClient
createClient CreateClient {..} = do
  validClientName <- validateName clientName
  pure ValidCreateClient {..}

validateName :: Text -> Either ValidationErrors Text
validateName = mapValError "name" . validateText [notEmpty]


