module Tripper.Shared.Types where

import RIO hiding (first)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap, first)
import Data.UUID (UUID)
import Database.Persist.Sql
  ( PersistField
  , PersistFieldSql
  , PersistValue (..)
  , SqlType (..)
  , sqlType
  , toPersistValue
  , fromPersistValue
  )
import Tripper.Shared.Validators.Error (IsErrorMsg)
import Tripper.Shared.Validators.Text
  ( contains
  , notEmpty
  , validateText
  , TextError
  )
import Web.PathPieces (PathPiece, fromPathPiece, toPathPiece) 

import qualified Data.UUID as U

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
  deriving newtype (FromJSON, ToJSON)

mkPassword :: Text -> RIO env Password
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

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . U.toASCIIBytes
  fromPersistValue (PersistDbSpecific v) = 
    case U.fromASCIIBytes v of
      Nothing -> Left "Invalid UUID"
      Just u  -> Right u
  fromPersistValue _ = Left "NotPersistDbSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  fromPathPiece = U.fromText
  toPathPiece = U.toText

