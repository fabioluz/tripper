module Tripper.Feature.Client.Server (ClientAPI, clientServer) where

import RIO
import Servant
import Tripper.Config
import Tripper.Feature.Client.DB
import Tripper.Feature.Client.Types
import Tripper.Feature.Shared
import Tripper.Feature.User.DB
import Tripper.Feature.User.Types

type ClientAPI
  = "clients"
    :> ReqBody '[JSON] CreateClient
    :> PostCreated '[JSON] NoContent

clientServer :: ServerT ClientAPI (AppM Config)
clientServer = createHandler

createHandler :: CreateClient -> AppM Config NoContent
createHandler clientInput = do
  let adminInput = fromCreateClient clientInput
  client     <- createClient clientInput `orThrow` http422
  admin      <- createUser adminInput `orThrow` http422
  emailInUse <- isJust <$> getUserByEmail (email adminInput)

  when emailInUse do
    throwIO $ http422 emailInUseError

  void $ insertClientAndAdmin client admin
  pure NoContent

emailInUseError :: ValidationErrors
emailInUseError = mkValError "email" "ALREADY_IN_USE"