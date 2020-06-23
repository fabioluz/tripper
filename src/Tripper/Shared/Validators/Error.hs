module Tripper.Shared.Validators.Error where

import Data.Aeson (ToJSON)
import Data.Bifunctor (first)
import RIO hiding (first)

import qualified RIO.HashMap as HM

class IsErrorMsg a where
  errorMsg :: a -> Text

instance {-# OVERLAPPABLE #-} Show a => IsErrorMsg a where
  errorMsg = tshow

instance IsErrorMsg Text where
  errorMsg = id

newtype ValidationErrors = ValidationErrors { unValError :: HashMap Text Text }
  deriving Generic
  deriving newtype ToJSON

mkValError :: Text -> Text -> ValidationErrors
mkValError key = ValidationErrors . HM.singleton key

mapValError :: IsErrorMsg a => Text -> Either a b -> Either ValidationErrors b
mapValError key = first $ mkValError key . errorMsg