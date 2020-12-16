module Tripper.Feature.Translation.Server (TranslationAPI, translationServer) where

import Data.Aeson
import RIO
import RIO.Directory
import RIO.FilePath
import Servant
import Tripper.Config

import qualified RIO.Text as T
import qualified RIO.HashMap as HM

-- The contract of the Translation API. This API is public
type TranslationAPI
  = "translation"
    :> Capture "lang" Text
    :> Get '[JSON] Object

translationServer :: ServerT TranslationAPI (AppM Config)
translationServer = getTranslationHandler

getTranslationHandler :: Text -> AppM Config Object
getTranslationHandler lang = do
  dir      <- getCurrentDirectory
  contents <- tryIO . liftIO . decodeFileStrict' $ dir </> "translations" </> langFileName lang
  case contents of
    Left e -> do
      logError $ display e
      throwIO err500
    Right o -> do
      pure $ fromMaybe HM.empty o

langFileName :: Text -> FilePath
langFileName lang = case T.toUpper lang of
  "EN" -> "EN.json"
  _    -> "EN.json"