module Tripper.Feature.Client.Server (ClientAPI, clientServer) where

import RIO
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Feature.Auth.Types
import Tripper.Feature.Client.DB
import Tripper.Feature.Client.Types
import Tripper.Feature.Shared
import Tripper.Feature.User.Types

type ClientAPI
  = "clients"
    :> ReqBody '[JSON] CreateClient
    :> PostCreated '[JSON] NoContent

clientServer :: AuthResult CurrentUser -> ServerT ClientAPI (RIO Config)
clientServer _ = createHandler

createHandler :: CreateClient -> RIO Config NoContent
createHandler clientInput = do
  let adminInput = fromCreateClient clientInput
  client <- createClient clientInput `orThrow` http422
  admin  <- createUser adminInput `orThrow` http422
  insertClientAndAdmin client admin
  pure NoContent
