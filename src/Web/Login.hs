{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards#-}

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
import Web.Common
import Web.Routes.Happstack()
import Data.Text hiding (map, zip, concatMap)
import Happstack.Auth
       (AuthState, AuthProfileURL(..), AuthURL(..))
import Data.Acid (AcidState)
import Happstack.Auth.Core.Profile
import Data.Acid.Advanced (query', update')
default (Integer, Double, Data.Text.Text)

-- | function which generates the homepage
homePage :: (TVar Session) -> RoutedNomyxServer Response
homePage ts = do
   (T.Session _ _ (Acid acidAuth acidProfile _)) <- liftRouteT $ lift $ readTVarIO ts
   do mUserId <- getUserId acidAuth acidProfile
      case mUserId of
         Nothing ->
            do loginURL <- showURL (U_AuthProfile $ AuthURL A_Login)
               mainPage'  "Login to Nomyx"
                          "Login to Nomyx"
                          (H.div $ p $ do
                             "Welcome to Nomyx! You can login "
                             H.a ! href (toValue loginURL) $ "here.")
                          True
         (Just (UserId uid)) -> do
            link <- showURL $ Noop $ fromInteger uid
            seeOther link (toResponse $ string "to game page")

handleProfileData :: AcidState AuthState
                  -> AcidState ProfileState
                  -> AcidState ProfileDataState
                  -> ProfileDataURL
                  -> (TVar Session)
                  -> RoutedNomyxServer Response
handleProfileData authStateH profileStateH profileDataStateH url ts =
    case url of
      CreateNewProfileData ->
          do mUserId <- getUserId authStateH profileStateH
             case mUserId of
               Nothing -> internalServerError $ toResponse $ string "not logged in."
               (Just (UserId userID)) -> do
                  s <- liftRouteT $ lift $ atomically $ readTVar ts
                  update' (acidProfileData $ _acid s) (SetProfileData (ProfileData (fromInteger userID) defaultPlayerSettings Nothing Nothing))
                  link <- showURL $ PSettings $ fromInteger userID
                  seeOther link (toResponse $ string "to settings page")
                  --toResponse <$> settings (fromInteger userID) ts
      (ViewProfileData uid) ->
          do mProfileData <- query' profileDataStateH (AskProfileData (fromInteger $ unUserId uid))
             ok $ toResponse $ show mProfileData
