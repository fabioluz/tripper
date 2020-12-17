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
    :> Post '[JSON] LoginOutput

authServer :: JWTSettings -> ServerT AuthAPI (AppM Config)
authServer = loginHandler

-- | Checks input credentials and generates JWT token if they are valid.
-- | Otherwise, returns 401
loginHandler :: JWTSettings -> Login -> AppM Config LoginOutput
loginHandler jwts input = do
  token <- authenticateUser jwts input
  case token of
    Nothing -> throwIO http401
    Just t  -> pure t

-- | Validates input credentials against database.
-- | Returns Token if credentials are valid.
-- | Otherwise, returns Nothing.
authenticateUser :: JWTSettings -> Login -> AppM Config (Maybe LoginOutput)
authenticateUser jwts Login {..} = runMaybeT do
  user    <- MaybeT $ DB.getUserByEmail email
  curUser <- MaybeT $ challengePassword password user
  token   <- MaybeT $ mkToken jwts curUser
  pure $ LoginOutput token user

  
-- | Validates plain password against user's password.
-- | Returns the current user object if password matches.
-- | Otherwise, returns nothing.
challengePassword :: Text -> Entity User -> AppM env (Maybe CurrentUser)
challengePassword inputPassword userEntity = pure 
  if checkPassword inputPassword userPassword
    then Just (mkCurrentUser userEntity)
    else Nothing
  where
    Entity _ User { userPassword } = userEntity

-- | Token expiration date
tokenExpDate :: AppM env UTCTime
tokenExpDate = addUTCTime (43800 * 60) <$> getCurrentTime
 
-- | Make JWT Token 
mkToken :: JWTSettings -> CurrentUser -> AppM env (Maybe ByteString)
mkToken jwts curUser = do
  expDate <- tokenExpDate
  token   <- liftIO $ makeJWT curUser jwts (Just expDate)
  pure $ either (const Nothing) (Just . toStrictBytes) token
  