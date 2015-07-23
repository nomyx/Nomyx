{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Nomyx.Web.Login where


import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map, label, br)
import Text.Blaze.Html5.Attributes hiding (dir, label)
import qualified Text.Blaze.Html5 as H
import Control.Monad.State
import Control.Concurrent.STM
import Control.Applicative
import Happstack.Server
import Web.Routes.Happstack()
import Web.Routes.RouteT
import Data.Text hiding (map, zip, concatMap)
import Data.Maybe
import Happstack.Authenticate.Core (AuthenticateURL(..), getUserId, )
import Facebook (Credentials(..))
import Nomyx.Core.Profile
import Nomyx.Core.Types as T hiding (PlayerSettings)
import Nomyx.Core.Session as S
import Nomyx.Web.Common

default (Integer, Double, Data.Text.Text)

-- | function which generates the homepage
notLogged :: TVar Session -> RoutedNomyxServer Response
notLogged ts = do
   (T.Session _ _ (Profiles acidAuth acidProfile)) <- liftIO $ readTVarIO ts
   do mUserId <- getUserId acidAuth
      case mUserId of
         Nothing ->
            do loginURL <- showURL (Auth $ Controllers)
               mainPage' "Nomyx"
                         "Not logged in"
                         (H.div $ p $ do
                            "Welcome to Nomyx! You can login "
                            H.a ! href (toValue loginURL) $ "here.")
                         True
         (Just _) -> do
            link <- showURL MainPage
            seeOther link $ toResponse $ ("to game page"::String)

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


authenticate :: AuthenticateURL -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> TVar Session -> RoutedNomyxServer Response
authenticate authURL routeAuthenticate ts = do
   nestURL Auth $ routeAuthenticate authURL
   --(T.Session _ _ Profiles{..}) <- liftIO $ atomically $ readTVar ts
   --postPickedURL <- showURL PostAuth
   --nestURL Auth $ handleAuthProfile acidAuth acidProfile appTemplate Nothing Nothing postPickedURL authProfileURL

facebookAuth =
    Credentials {appName = "Nomyx",
                 appId = "161007670738608",
                 appSecret = "c0509c1c753f89d1d1fc181984042824"}
