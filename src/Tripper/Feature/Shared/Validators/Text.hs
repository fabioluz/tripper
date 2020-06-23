module Tripper.Feature.Shared.Validators.Text
( TextError
, contains
, gtOrEq
, minLength
, notEmpty
, validateText
) where

import RIO
import Tripper.Feature.Shared.Validators.Error

import qualified RIO.Text as T

newtype TextError = TextError { unTextError :: Text }
  deriving newtype IsErrorMsg

validate :: Text -> (Text -> Bool) -> Text -> Either TextError Text
validate e f x
  | f x       = Right x
  | otherwise = Left $ TextError e

validateText :: [Text -> Either TextError Text] -> Text -> Either TextError Text
validateText vals x = foldl' (>>=) (Right x) vals

gtOrEq :: Int -> Int -> Bool
gtOrEq n x = x >= n

minLength :: Int -> Text -> Either TextError Text
minLength n = validate errorMsg $ gtOrEq n . T.length
  where
    errorMsg = "MIN_LENGTH " <> tshow n

notEmpty :: Text -> Either TextError Text
notEmpty = validate errorMsg $ not . T.null
  where
    errorMsg = "NOT_EMTPY"

contains :: Text -> Text -> Either TextError Text
contains x = validate errorMsg $ T.isInfixOf x
  where
    errorMsg = "CONTAINS " <> x

