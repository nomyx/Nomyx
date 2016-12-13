{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Nomyx.Auth.Auth where

import           Nomyx.Auth.Types
import           Nomyx.Language
import           Happstack.Authenticate.OpenId.Route   (initOpenId)
import           Happstack.Authenticate.Password.Route (initPassword)
import           Happstack.Authenticate.Password.Core  (PasswordConfig(..))
import           Happstack.Authenticate.Route          (initAuthentication)
import           Happstack.Authenticate.Core           (AuthenticateConfig(..),
                                                        User(..),
                                                        UserId(..),
                                                        GetUserByUserId (..),
                                                        usernamePolicy,
                                                        getUserId)
import           Happstack.Server
import           Data.Maybe
import           Control.Monad.State
import           Data.Acid.Advanced                  (query')
import           Data.Text as T

launchAuth :: FilePath -> IO AuthState
launchAuth sd = do
   let authenticateConfig = AuthenticateConfig
               { _isAuthAdmin        = const $ return True
               , _usernameAcceptable = usernamePolicy
               , _requireEmail       = True
               }
   let passwordConfig = PasswordConfig
               { _resetLink = "http://localhost:8000/#resetPassword"
               , _domain    =  "example.org"
               , _passwordAcceptable = \t ->
                   if T.length t >= 5
                   then Nothing
                   else Just "Must be at least 5 characters."
               }
   --init authenticate
   (_, routeAuthenticate, authenticateState) <- liftIO $
      initAuthentication (Just sd) authenticateConfig [initPassword passwordConfig, initOpenId]
   return $ AuthState authenticateState routeAuthenticate

-- | return the player number (user ID) based on the session cookie.
getPlayerNumber :: AuthState -> ServerPartT IO (Maybe PlayerNumber)
getPlayerNumber (AuthState auth _) = do
   uid <- getUserId auth
   case uid of
      Nothing -> return Nothing
      (Just (UserId userID)) -> return $ Just $ fromInteger userID

getUser :: AuthState -> ServerPartT IO (Maybe User)
getUser (AuthState auth _) = do
  userId <- getUserId auth
  liftIO $ query' auth (GetUserByUserId $ fromJust userId)

