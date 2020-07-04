module Tripper.Feature.Shared.Utils where

import RIO

toRIO :: a -> RIO env a
toRIO = liftIO . pure