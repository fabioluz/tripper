module Tripper.Feature.Shared.Error where

import Data.Aeson
import RIO
import Servant
import Tripper.Feature.Shared.Validators.Error

lbshow :: Show a => a -> LByteString
lbshow = fromStrictBytes . encodeUtf8 . tshow

toEither :: Maybe a -> Either () a
toEither = maybe (Left ()) Right

orThrow :: Either e a -> (e -> ServerError) -> RIO env a
orThrow e f = either (throwIO . f) pure e

http401 :: ServerError
http401 = err401

http404 :: ServerError
http404 = err404

http422 :: ValidationErrors -> ServerError
http422 errors = err422 { errBody = encode errors }

http500 :: Show e => e -> ServerError
http500 e = err500 { errBody = lbshow e }

