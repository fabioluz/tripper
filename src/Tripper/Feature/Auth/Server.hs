module Tripper.Feature.Auth.Server (AuthAPI, authServer) where

import RIO
import RIO.Time
import Servant
import Servant.Auth.Server
import Tripper.Config
import Tripper.Feature.Auth.DB
import Tripper.Feature.Auth.Types

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
  mayUser <- getUserByCredentials login
  case mayUser of
    Nothing   -> throwIO err401
    Just user -> do
      cs         <- updateCS baseCS
      mayCookies <- liftIO $ acceptLogin cs jwts (mkCurrentUser user)
      case mayCookies of
        Nothing      -> throwIO err401
        Just cookies -> pure $ cookies NoContent

-- | Updates cookie settings' expiration time.
-- | This is also used in JWT tokens internally.
updateCS :: CookieSettings -> RIO env CookieSettings
updateCS cs = do
  now <- getCurrentTime
  let exp = Just $ addUTCTime (43800 * 60) now
  pure $ cs { cookieExpires = exp }

