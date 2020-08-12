module Tripper.Feature.Auth.Server (AuthAPI, authServer) where

import Control.Monad.Trans.Maybe
import Control.Monad.Except
import RIO
import RIO.Time
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Feature.Auth.Types
import Tripper.Feature.Shared
import Tripper.Models

import qualified Tripper.Feature.User.DB as DB

-- The contract of the Authentication API. This API is public
type AuthAPI
  = "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] Token

authServer :: JWTSettings -> ServerT AuthAPI (RIO Config)
authServer = loginHandler

-- | Checks input credentials and generates JWT token if they are valid.
-- | Otherwise, returns 401
loginHandler :: JWTSettings -> Login -> RIO Config Token
loginHandler jwts input = do
  token <- authenticateUser jwts input
  case token of
    Nothing -> throwIO http401
    Just t  -> pure t

-- | Validates input credentials against database.
-- | Returns Token if credentials are valid.
-- | Otherwise, returns Nothing.
authenticateUser :: JWTSettings -> Login -> RIO Config (Maybe Token)
authenticateUser jwts Login {..} = runMaybeT do
  user    <- MaybeT $ DB.getUserByEmail email
  curUser <- MaybeT $ challengePassword password user
  token   <- MaybeT $ mkToken jwts curUser
  pure token

  
-- | Validates plain password against user's password.
-- | Returns the current user object if password matches.
-- | Otherwise, returns nothing.
challengePassword :: Text -> Entity User -> RIO env (Maybe CurrentUser)
challengePassword inputPassword userEntity = pure 
  if checkPassword inputPassword userPassword
    then Just (mkCurrentUser userEntity)
    else Nothing
  where
    Entity _ User { userPassword } = userEntity

-- | Token expiration date
expDate :: RIO env UTCTime
expDate = addUTCTime (43800 * 60) <$> getCurrentTime
 
-- | Make JWT Token 
mkToken :: JWTSettings -> CurrentUser -> RIO env (Maybe Token)
mkToken jwts curUser = do
  exp <- expDate
  jwt <- liftIO $ makeJWT curUser jwts (Just exp)
  let token = either (const Nothing) Just jwt
  pure $ Token <$> token