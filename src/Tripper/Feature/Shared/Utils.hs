module Tripper.Feature.Shared.Utils where

import RIO
import Tripper.Config

toRIO :: a -> AppM env a
toRIO = liftIO . pure