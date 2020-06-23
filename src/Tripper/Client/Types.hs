module Tripper.Client.Types
( CreateClient (..)
, createClient
, ValidCreateClient (..)
) where

import RIO
import Data.Aeson (FromJSON, ToJSON)
import Tripper.Shared.Validators.Error (mapValError, ValidationErrors)
import Tripper.Shared.Validators.Text (validateText, notEmpty)

data CreateClient = CreateClient
  { clientName    :: Text
  , adminEmail    :: Text
  , adminPassword :: Text
  , adminName     :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data ValidCreateClient = ValidCreateClient
  { validClientName :: Text }

createClient :: CreateClient -> Either ValidationErrors ValidCreateClient
createClient CreateClient {..} = do
  validClientName <- validateName clientName
  pure ValidCreateClient {..}

validateName :: Text -> Either ValidationErrors Text
validateName = mapValError "name" . validateText [notEmpty]

