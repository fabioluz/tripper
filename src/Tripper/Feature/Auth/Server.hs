module Tripper.Feature.Auth.Server (AuthAPI, authServer) where

import Control.Monad.Trans.Maybe
import Database.Persist
import RIO
import RIO.Time
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.DB
import Tripper.Feature.Auth.Types
import Tripper.Feature.Shared
import Tripper.Feature.User.DB
import Tripper.Models

-- Represets the response type for the authorization endpoint
type AuthResponse = Headers '[ Header "Set-Cookie" SetCookie
                             , Header "Set-Cookie" SetCookie
                             ] NoContent

-- The contract of the Authentication API. This API is public
type AuthAPI
  = "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] AuthResponse

authServer :: HasConfig env => CookieSettings -> JWTSettings -> ServerT AuthAPI (RIO env)
authServer = loginHandler

-- | This checks user's credentials and generates JWT token if it is valid.
-- | Returs 401 if crendetials are not valid
loginHandler :: HasConfig env => CookieSettings -> JWTSettings -> Login -> RIO env AuthResponse
loginHandler baseCS jwts login = do
  cs      <- updateCS baseCS
  authRes <- authUser cs jwts login
  case authRes of
    Nothing      -> throwIO err401
    Just cookies -> pure $ cookies NoContent

-- | Updates cookie settings' expiration time.
-- | This is also used in JWT tokens internally.
updateCS :: CookieSettings -> RIO env CookieSettings
updateCS cs = do
  now <- getCurrentTime
  let exp = Just $ addUTCTime (43800 * 60) now
  pure $ cs { cookieExpires = exp }

-- | Validate the input password againts the entity user.
validatePassword :: Text -> Entity User -> RIO env (Maybe CurrentUser)
validatePassword password user = pure $
   if checkPassword password currentPassword
      then Just $ mkCurrentUser user
      else Nothing
  where
    currentPassword = userPassword $ entityVal user

-- | Tries to authenticate the user (if the credentials match) and create response cookie with token.
authUser
  :: ( HasPool env
     , AddHeader "Set-Cookie" SetCookie response withOneCookie
     , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies
     )
  => CookieSettings
  -> JWTSettings
  -> Login
  -> RIO env (Maybe (response -> withTwoCookies))
authUser cs jwts Login {..} = runMaybeT $ do
  user    <- MaybeT $ getUserByEmail email
  current <- MaybeT $ validatePassword password user
  cookies <- MaybeT $ liftIO (acceptLogin cs jwts current)
  pure cookies

