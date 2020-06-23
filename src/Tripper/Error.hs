module Tripper.Error where

import RIO
import Data.Aeson (encode)
import Servant (ServerError (..), err422, err500)
import Tripper.Shared.Validators.Error (ValidationErrors (..))

lbshow :: Show a => a -> LByteString
lbshow = fromStrictBytes . encodeUtf8 . tshow

orThrow :: Either e a -> (e -> ServerError) -> RIO env a
orThrow e f = either (throwIO . f) pure e

http422 :: ValidationErrors -> ServerError
http422 errors = err422 { errBody = encode errors }

http500 :: Show e => e -> ServerError
http500 e = err500 { errBody = lbshow e }