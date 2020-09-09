module Tripper.Feature.Shared.Types where

import Crypto.KDF.BCrypt
import Data.Aeson
import Data.Bifunctor
import Data.UUID
import Database.Persist.Sql
import RIO hiding (first)
import Tripper.Config
import Tripper.Feature.Shared.Validators.Error
import Tripper.Feature.Shared.Validators.Text
import Web.HttpApiData
import Web.PathPieces

-- |
-- | Email
-- |

newtype Email = Email { unEmail :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype EmailError = EmailError { unEmailError :: TextError }
  deriving newtype (IsErrorMsg)

mkEmail :: Text -> Either EmailError Email
mkEmail = bimap EmailError Email . validateText [notEmpty, contains "@"]

instance PersistField Email where
  toPersistValue = PersistText . unEmail
  fromPersistValue = \case
    PersistText v -> Right $ Email v
    _             -> Left  $ "NotValidEmailType"

instance PersistFieldSql Email where
  sqlType _ = SqlString

instance PathPiece Email where
  fromPathPiece = Just . Email
  toPathPiece = unEmail

-- |
-- | Password
-- |

newtype Password = Password { unPassword :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON)

mkPassword :: Text -> AppM env Password
mkPassword plain = do
  hash <- liftIO $ hashPassword 10 (encodeUtf8 plain)
  pure $ Password (decodeUtf8Lenient hash)

checkPassword :: Text -> Password -> Bool
checkPassword plain = validatePassword (encodeUtf8 plain) . encodeUtf8 . unPassword

instance PersistField Password where
  toPersistValue = PersistText . unPassword
  fromPersistValue = \case
    PersistText v -> Right $ Password v
    _             -> Left  $ "NotValidPasswordType"

instance PersistFieldSql Password where
  sqlType _ = SqlString

-- |
-- | UUID
-- |

newtype GUID = GUID { unGUID :: UUID }
  deriving newtype
    ( Eq
    , Ord
    , Show
    , Read
    , FromJSON
    , ToJSON
    , FromHttpApiData
    , ToHttpApiData
    )

instance PersistField GUID where
  toPersistValue = PersistDbSpecific . toASCIIBytes . unGUID
  fromPersistValue (PersistDbSpecific v) =
    case fromASCIIBytes v of
      Nothing -> Left  $ "Invalid UUID"
      Just u  -> Right $ GUID u
  fromPersistValue _ = Left "NotPersistDbSpecific"

instance PersistFieldSql GUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece GUID where
  fromPathPiece = fmap GUID . fromText
  toPathPiece = toText . unGUID

