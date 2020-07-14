module Tripper.Feature.Client.Server (ClientAPI, clientServer) where

import RIO
import Servant
import Tripper.Config
import Tripper.Feature.Client.DB
import Tripper.Feature.Client.Types
import Tripper.Feature.Shared
import Tripper.Feature.User.Types

type ClientAPI
  = "clients"
    :> ReqBody '[JSON] CreateClient
    :> PostCreated '[JSON] NoContent

clientServer :: ServerT ClientAPI (RIO Config)
clientServer = createHandler

createHandler :: CreateClient -> RIO Config NoContent
createHandler clientInput = do
  let adminInput = fromCreateClient clientInput
  client <- createClient clientInput `orThrow` http422
  admin  <- createUser adminInput `orThrow` http422
  insertClientAndAdmin client admin
  pure NoContent
