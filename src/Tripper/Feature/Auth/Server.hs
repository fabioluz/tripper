module Tripper.Feature.Auth.Server (AuthAPI, authServer) where

import Control.Monad.Trans.Maybe
import RIO
import RIO.Time
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Feature.Auth.Types
import Tripper.Feature.Shared
import Tripper.Models

import qualified Tripper.Feature.User.DB as DB

-- Represets the response type for the authorization endpoint
type AuthResponse = Headers '[ Header "Set-Cookie" SetCookie
                             , Header "Set-Cookie" SetCookie
                             ] NoContent

-- The contract of the Authentication API. This API is public
type AuthAPI = "login" :> ReqBody '[JSON] Login :> Post '[JSON] AuthResponse

authServer :: HasConfig env => CookieSettings -> JWTSettings -> ServerT AuthAPI (RIO env)
authServer = loginHandler

-- | Checks input credentials and generates JWT token if they are valid.
-- | Otherwise, returns 401
loginHandler :: HasConfig env => CookieSettings -> JWTSettings -> Login -> RIO env AuthResponse
loginHandler baseCS jwts input = do
  mayCookies <- authenticateUser baseCS jwts input
  case mayCookies of
    Nothing      -> throwIO http401
    Just cookies -> pure $ cookies NoContent

-- | Validates input credentials against database.
-- | Returns SetCookie function with JWT token if credentials are valid.
-- | Returns nothing, otherwise.
authenticateUser 
  :: ( HasConfig env
     , AddHeader "Set-Cookie" SetCookie response withOneCookie
     , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies
     )
  => CookieSettings
  -> JWTSettings
  -> Login
  -> RIO env (Maybe (response -> withTwoCookies))
authenticateUser baseCS jwts Login {..} = runMaybeT $ do
  user    <- MaybeT $ DB.getUserByEmail email
  curUser <- MaybeT $ challengePassword password user
  cs      <- MaybeT $ Just <$> updateCS baseCS
  MaybeT $ liftIO $ acceptLogin cs jwts curUser

  
-- | Validates plain password against user's password.
-- | Returns the current user object if password matches.
-- | Otherwise, returns nothing.
challengePassword :: Text -> Entity User -> RIO env (Maybe CurrentUser)
challengePassword inputPassword userEntity = pure $
  if checkPassword inputPassword userPassword
    then Just (mkCurrentUser userEntity)
    else Nothing
  where
    Entity _ User { userPassword } = userEntity

-- | Updates cookie settings' expiration time.
-- | This is also used in JWT tokens internally.
updateCS :: CookieSettings -> RIO env CookieSettings
updateCS cs = do
  now <- getCurrentTime
  let exp = Just $ addUTCTime (43800 * 60) now
  pure $ cs { cookieExpires = exp }

