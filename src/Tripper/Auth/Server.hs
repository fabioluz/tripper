module Tripper.Auth.Server (AuthAPI, authServer) where

import RIO
import RIO.Time (getCurrentTime, addUTCTime)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Database.Persist (Entity (..))
import Servant 
  ( AddHeader
  , Header
  , Headers
  , JSON
  , NoContent (..)
  , Post
  , ReqBody
  , ServerT
  , (:>)
  , addHeader
  , err401
  )
import Servant.Auth.Server
  ( Auth
  , CookieSettings
  , JWT
  , JWTSettings
  , SetCookie
  , cookieExpires
  , acceptLogin
  )
import Tripper.DB (HasPool)
import Tripper.Config (HasConfig)
import Tripper.Models (User (..))
import Tripper.Auth.Types (CurrentUser, mkCurrentUser, Login (..))
import Tripper.Shared.Types (checkPassword)
import Tripper.User.DB (getUserByEmail)

-- Represents the headers for the authorization response
type AuthHeaders = '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

-- Represets the response type for the authorization endpoint
type AuthResponse = Headers AuthHeaders NoContent

-- The contract of the Authentication API. This API is public
type AuthAPI = "login" :> ReqBody '[JSON] Login :> Post '[JSON] AuthResponse

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

-- | Updates cookie settings' expiration time
-- | This is also used in JWT tokens internally
updateCS :: CookieSettings -> RIO env CookieSettings
updateCS cs = do
  now <- getCurrentTime
  let exp = Just $ addUTCTime (43800 * 60) now
  pure $ cs { cookieExpires = exp }


validatePassword :: Text -> Entity User -> RIO env (Maybe CurrentUser)
validatePassword password user = pure $
   if checkPassword password currentPassword
      then Just $ mkCurrentUser user
      else Nothing
  where
    currentPassword = userPassword $ entityVal user

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
  
