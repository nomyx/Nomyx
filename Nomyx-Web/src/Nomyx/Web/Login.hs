{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Nomyx.Web.Login where


import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map, label, br)
import Text.Blaze.Html5.Attributes hiding (dir, label)
import qualified Text.Blaze.Html5 as H
import Web.Routes.RouteT
import Text.Blaze.Internal
import Control.Monad.State
import Control.Concurrent.STM
import Happstack.Server
import Web.Routes.Happstack()
import Data.Text hiding (map, zip, concatMap)
import Happstack.Auth (AuthProfileURL(..), AuthURL(..), handleAuthProfile)
import Happstack.Auth.Core.Profile
import Facebook (Credentials(..))
import Nomyx.Core.Profile
import Nomyx.Core.Types as T hiding (PlayerSettings)
import Nomyx.Core.Session as S
import Nomyx.Web.Common

default (Integer, Double, Data.Text.Text)

-- | function which generates the homepage
notLogged :: TVar Session -> RoutedNomyxServer Response
notLogged ts = do
   (T.Session _ _ (Profiles acidAuth acidProfile _)) <- liftIO $ readTVarIO ts
   do mUserId <- getUserId acidAuth acidProfile
      case mUserId of
         Nothing ->
            do loginURL <- showURL (Auth $ AuthURL A_Login)
               mainPage' "Nomyx"
                         "Not logged in"
                         (H.div $ p $ do
                            "Welcome to Nomyx! You can login "
                            H.a ! href (toValue loginURL) $ "here.")
                         True
         (Just _) -> do
            link <- showURL MainPage
            seeOther link (toResponse $ string "to game page")

-- | add a new player if not existing
postAuthenticate :: TVar Session -> RoutedNomyxServer Response
postAuthenticate ts = do
   pn <- getPlayerNumber ts
   pf <- getProfile' ts pn
   case pf of
      Just _ -> do
         link <- showURL MainPage
         seeOther link (toResponse $ string "to main page")
      Nothing -> do
         webCommand ts $ S.newPlayer pn defaultPlayerSettings
         link <- showURL PlayerSettings
         seeOther link (toResponse $ string "to settings page")


authenticate :: AuthProfileURL -> TVar Session -> RoutedNomyxServer Response
authenticate authProfileURL ts = do
   (T.Session _ _ Profiles{..}) <- liftIO $ atomically $ readTVar ts
   postPickedURL <- showURL PostAuth
   nestURL Auth $ handleAuthProfile acidAuth acidProfile appTemplate Nothing Nothing postPickedURL authProfileURL

facebookAuth =
    Credentials {appName = "Nomyx",
                 appId = "161007670738608",
                 appSecret = "c0509c1c753f89d1d1fc181984042824"}
