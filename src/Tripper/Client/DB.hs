module Tripper.Client.DB (insertClientAndAdmin) where

import RIO
import RIO.Time
import Database.Persist
import Tripper.Client.Types
import Tripper.DB 
import Tripper.Error
import Tripper.Models
import Tripper.Feature.Shared
import Tripper.User.DB
import Tripper.User.Types

insertClientAndAdmin :: HasPool env => ValidCreateClient -> ValidCreateUser -> RIO env ()
insertClientAndAdmin validClient validUser = runDb $ do
  client   <- liftRIO $ mkClient validClient
  clientId <- insert client

  user       <- liftRIO $ mkUser clientId validUser
  emailInUse <- isJust <$> checkUnique user
  when emailInUse $ do
    throwIO $ http422 emailInUseError

  userId <- insert user
  pure ()

mkClient :: ValidCreateClient -> RIO env Client
mkClient ValidCreateClient {..} = do
  now <- getCurrentTime
  pure Client
    { clientName      = validClientName
    , clientCreatedAt = now
    , clientUpdatedAt = now
    }

emailInUseError :: ValidationErrors
emailInUseError = mkValError "email" "ALREADY_IN_USE"
