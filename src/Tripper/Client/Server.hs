module Tripper.Client.Server where

import RIO
import Servant (JSON, NoContent (..), PostCreated, ReqBody, ServerT, (:>))
import Servant.Auth.Server (AuthResult)
import Tripper.Auth.Types (CurrentUser)
import Tripper.Config (HasConfig)
import Tripper.Error (orThrow, http422)
import Tripper.Client.Types (CreateClient, createClient)
import Tripper.Client.DB (insertClientAndAdmin)
import Tripper.User.Types (CreateUser, fromCreateClient, createUser)

type ClientAPI
  = "clients"
    :> ReqBody '[JSON] CreateClient
    :> PostCreated '[JSON] NoContent

clientServer :: HasConfig env => AuthResult CurrentUser -> ServerT ClientAPI (RIO env)
clientServer _ = createHandler

createHandler :: HasConfig env => CreateClient -> RIO env NoContent
createHandler clientInput = do
  let adminInput = fromCreateClient clientInput
  client <- createClient clientInput `orThrow` http422
  admin  <- createUser adminInput `orThrow` http422
  insertClientAndAdmin client admin
  pure NoContent