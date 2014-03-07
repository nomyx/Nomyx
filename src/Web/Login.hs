{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Login where


import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map, label, br)
import Text.Blaze.Html5.Attributes hiding (dir, label)
import qualified Text.Blaze.Html5 as H
import Web.Routes.RouteT
import Text.Blaze.Internal
import Control.Monad.State
import Control.Concurrent.STM
import Happstack.Server
import Types as T
import Session as S
import Web.Common
import Web.Routes.Happstack()
import Data.Text hiding (map, zip, concatMap)
import Happstack.Auth (AuthProfileURL(..), AuthURL(..), handleAuthProfile)
import Happstack.Auth.Core.Profile
import Facebook (Credentials(..))
import Profile
default (Integer, Double, Data.Text.Text)

-- | function which generates the homepage
homePage :: (TVar Session) -> RoutedNomyxServer Response
homePage ts = do
   (T.Session _ _ (Profiles acidAuth acidProfile _)) <- liftIO $ readTVarIO ts
   do mUserId <- getUserId acidAuth acidProfile
      case mUserId of
         Nothing ->
            do loginURL <- showURL (U_AuthProfile $ AuthURL A_Login)
               mainPage'  "Nomyx"
                          "Not logged in"
                          (H.div $ p $ do
                             "Welcome to Nomyx! You can login "
                             H.a ! href (toValue loginURL) $ "here.")
                          True
         (Just _) -> do
            link <- showURL MainPage
            seeOther link (toResponse $ string "to game page")

-- | add a new player if not existing
postAuthenticate :: (TVar Session) -> RoutedNomyxServer Response
postAuthenticate ts = do
   pn <- getPlayerNumber ts
   pf <- getProfile' ts pn
   case pf of
      Just _ -> do
         link <- showURL $ MainPage
         seeOther link (toResponse $ string "to main page")
      Nothing -> do
         webCommand ts $ S.newPlayer pn defaultPlayerSettings
         link <- showURL $ Web.Common.PlayerSettings
         seeOther link (toResponse $ string "to settings page")


authenticate :: (TVar Session) -> AuthProfileURL -> RoutedNomyxServer Response
authenticate ts authProfileURL = do
   (T.Session _ _ Profiles{..}) <- liftIO $ atomically $ readTVar ts
   postPickedURL <- showURL PostAuth
   nestURL U_AuthProfile $ handleAuthProfile acidAuth acidProfile appTemplate (Just facebookAuth) Nothing postPickedURL authProfileURL

facebookAuth =
    Credentials {appName = "Nomyx",
                 appId = "161007670738608",
                 appSecret = "c0509c1c753f89d1d1fc181984042824"}
