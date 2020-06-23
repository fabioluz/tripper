module Tripper.Client.DB (insertClientAndAdmin) where

import RIO
import RIO.Time (getCurrentTime)
import Database.Persist (checkUnique, insert)
import Tripper.DB (HasPool, runDb)
import Tripper.Error (http422)
import Tripper.Models (Client (..), ClientId, User(..), Unique (..), EntityField (..))
import Tripper.Client.Types (ValidCreateClient (..))
import Tripper.User.DB (mkUser)
import Tripper.User.Types (ValidCreateUser (..))
import Tripper.Shared.Types (mkPassword)
import Tripper.Shared.Validators.Error (mkValError, ValidationErrors)

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