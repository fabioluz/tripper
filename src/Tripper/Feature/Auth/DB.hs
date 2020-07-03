module Tripper.Feature.Auth.DB where

import Database.Persist
import RIO
import Tripper.Config
import Tripper.Feature.Auth.Types
import Tripper.Feature.Shared
import Tripper.Models

getUserByCredentials :: HasPool env => Login -> RIO env (Maybe (Entity User))
getUserByCredentials Login {..} = runDb $ do
  mayUser <- getBy $ UniqueUserEmail $ Email email
  pure $ mayUser >>= \entityUser ->
    if checkPassword password (userPassword (entityVal entityUser))
      then Just entityUser
      else Nothing
