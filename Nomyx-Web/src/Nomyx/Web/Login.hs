{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Nomyx.Web.Login where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map, label, br)
import Text.Blaze.Html5.Attributes hiding (dir, label)
import Text.Blaze.Internal
import qualified Text.Blaze.Html5 as H
import Control.Monad.State
import Control.Concurrent.STM
import Control.Applicative
import Happstack.Server
import Web.Routes.Happstack()
import Web.Routes.RouteT
import Data.Text hiding (map, zip, concatMap)
import Data.Maybe
import Happstack.Authenticate.Core (AuthenticateURL(..), getUserId, AuthenticateState)
import Facebook (Credentials(..))
import Nomyx.Core.Profile
import Nomyx.Core.Types as T hiding (PlayerSettings)
import Nomyx.Core.Session as S
import Nomyx.Web.Common
import Data.Acid
import Language.Javascript.JMacro

default (Integer, Double, Data.Text.Text)

-- | function which generates the login page
loginPage :: TVar Session -> RoutedNomyxServer Response
loginPage ts = do
  postAuth <- showURL PostAuth
  mainPage' "Nomyx" "Login page" True $ do
      H.div ! customAttribute "ng-controller" "UsernamePasswordCtrl" $ do
        H.div ! customAttribute "up-authenticated" "false" $ do
          h2 "Login"
          customLeaf (stringTag "up-login-inline") True
        H.div ! customAttribute "up-authenticated" "true" $ do
          p "You have successfully logged in!"
          H.a "Click here to create a new player" ! (href $ toValue postAuth)
          h2 "Logout"
          p "Click the link below to logout."
          customLeaf (stringTag "up-logout") True
          h2 "Forgotten Password"
          p "Forgot your password? Request a reset link via email!"
          customLeaf (stringTag "up-request-reset-password") True
        h2 "Create A New Account"
        customLeaf (stringTag "up-signup-password") True

logout :: TVar Session -> RoutedNomyxServer Response
logout ts = do
  main <- showURL MainPage
  ok $ do
    H.div ! customAttribute "ng-controller" "UsernamePasswordCtrl" $ do
      customLeaf (stringTag "up-logout") True
  seeOther main $ toResponse $ ("to game page" :: String)

-- | add a new player if not existing
postAuthenticate :: TVar Session -> RoutedNomyxServer Response
postAuthenticate ts = do
   pn <- fromJust <$> getPlayerNumber ts
   pf <- getProfile' ts pn
   case pf of
      Just _ -> do
         link <- showURL MainPage
         seeOther link $ toResponse ("to main page" :: String)
      Nothing -> do
         webCommand ts $ S.newPlayer pn defaultPlayerSettings
         link <- showURL PlayerSettings
         seeOther link $ toResponse ("to settings page" :: String)


authenticate :: AuthenticateURL
            -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
            -> TVar Session
            -> RoutedNomyxServer Response
authenticate authURL routeAuthenticate ts = do
   nestURL Auth $ routeAuthenticate authURL
   --(T.Session _ _ Profiles{..}) <- liftIO $ atomically $ readTVar ts
   --postPickedURL <- showURL PostAuth
   --nestURL Auth $ handleAuthProfile acidAuth acidProfile appTemplate Nothing Nothing postPickedURL authProfileURL

facebookAuth =
    Credentials {appName = "Nomyx",
                 appId = "161007670738608",
                 appSecret = "c0509c1c753f89d1d1fc181984042824"}


changePasswordPanel :: TVar Session -> RoutedNomyxServer Response
changePasswordPanel _ = mainPage' "Nomyx" "Login page" True $ do
   customLeaf (stringTag "up-change-password") True

openIdRealmPanel :: TVar Session -> RoutedNomyxServer Response
openIdRealmPanel _ = mainPage' "Nomyx" "Login page" True $ do
   H.div ! customAttribute "ng-controller" "OpenIdCtrl" $ do
     customLeaf (stringTag "openid-realm") True


resetPasswordPage :: TVar Session -> RoutedNomyxServer Response
resetPasswordPage _ = mainPage' "Nomyx" "Login page" True $ do
   h2 "Reset Password"
   customLeaf (stringTag "up-reset-password") True
