module Tripper.Feature.Client.DB (insertClientAndAdmin) where

import Database.Persist
import RIO
import RIO.Time
import Tripper.Config
import Tripper.Feature.Client.Types
import Tripper.Feature.User.DB
import Tripper.Feature.User.Types
import Tripper.Models

insertClientAndAdmin :: HasPool env => ValidCreateClient -> ValidCreateUser -> AppM env (ClientId, UserId)
insertClientAndAdmin validClient validUser = runDb do
  client   <- liftAppM $ mkClient validClient
  clientId <- insert client
  user     <- liftAppM $ mkUser clientId validUser
  userId   <- insert user
  pure (clientId, userId)

mkClient :: ValidCreateClient -> AppM env Client
mkClient ValidCreateClient {..} = do
  now <- liftIO getCurrentTime
  pure Client
    { clientName      = validClientName
    , clientCreatedAt = now
    , clientUpdatedAt = now
    }
