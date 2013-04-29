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
import Language.Nomyx.Expression
import Utils
import Text.Reform.Happstack
import Text.Reform
import Debug.Trace.Helpers
import Happstack.Server
import Types
import Multi --TODO to remove
import Web.Common
import Web.Settings
import Web.Routes.Happstack()
import Control.Applicative
import Text.Reform.Blaze.String hiding (form)
import Data.Text hiding (map, zip, concatMap)
import Data.Lens
import Happstack.Auth (AuthProfileURL(..), AuthURL(..), getUserId)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text (pack)
import Data.Acid.Advanced    (update', query')
default (Integer, Double, Data.Text.Text)

-- | function which generates the homepage
homePage :: Acid -- ^ database handle
         -> RoutedNomyxServer Html -- RouteT SiteURL (ServerPartT IO) Response
homePage Acid{..} =
    do mUserId <- getUserId acidAuth acidProfile
       case mUserId of
         Nothing ->
             do loginURL <- showURL (U_AuthProfile $ AuthURL A_Login)
                mainPage (H.div $ p $ do
                            "You can login "
                            H.a ! href (toValue loginURL) $ "here.")
                         "not logged in."
                         "not logged in."
                         True
         (Just uid) -> do
             mpd <- query' acidProfileData (AskProfileData uid)
             logoutURL  <- showURL (U_AuthProfile $ AuthURL A_Logout)
             addAuthURL <- showURL (U_AuthProfile $ AuthURL A_AddAuth)
             mainPage (do
                  H.div $ do
                    H.p $ "You are logged in as" >> toHtml (show uid)
                    H.p $ "You can logout " >> (H.a ! href (toValue logoutURL) $ "here") >> "."
                    H.p $ "You can add an additional auth method ">> (H.a ! href (toValue addAuthURL) $ "here") >> "."
                    H.p $ "Your message is: " >> (toHtml $ fromMaybe (Text.pack "profile data missing.") (fmap profileMsg mpd)))
                "logged in."
                "logged in."
                True

loginForm :: Maybe LoginPass -> NomyxForm LoginPass
loginForm (Just (LoginPass login _)) = loginForm' login
loginForm Nothing = loginForm' ""

loginForm' :: String -> NomyxForm LoginPass
loginForm' login = LoginPass <$> br ++> label "Please enter your login and password (if you don't have one, just invent it):" ++> br ++> br
                   ++>  label "Login: " ++> (inputText login) <*> label "    Password: " ++> inputPassword <++ label " "


loginPage :: RoutedNomyxServer Html
loginPage = do
   link <- showURL PostLogin
   lf  <- lift $ viewForm "user" $ loginForm Nothing
   mainPage (blazeForm lf link)
             "Login to Nomyx"
             "Login to Nomyx"
             True

newPlayerPage :: LoginPass -> RoutedNomyxServer Html
newPlayerPage lp = do
   link <- showURL $ NewPlayerLogin lp
   lf  <- lift $ viewForm "user" $ loginForm (Just lp)
   mf  <- lift $ viewForm "user" $ settingsForm Nothing
   mainPage (do
                lf ! (disabled "")
                H.br >> H.br
                "New Player? Welcome!" >> H.br
                (blazeForm mf link))
             "Login to Nomyx"
             "New Player"
             True


newPlayerLogin :: (TVar Session) -> LoginPass -> RoutedNomyxServer Html
newPlayerLogin tm (LoginPass login password) = do
    methodM POST
    liftRouteT $ lift $ putStrLn $ "newPlayerLogin"
    r <- liftRouteT $ eitherForm environment "user" $ settingsForm Nothing
    case r of
       (Right ms) -> do
          mpn <- evalCommand tm $ checkLoginWeb login password
          case mpn of
             LoginOK pn -> do
                link <- showURL $ Noop pn
                webCommand tm pn $ mailSettings ms pn
                seeOther link $ string "Redirecting..."
             WrongPassword -> do
                link <- showURL $ Login
                seeOther link $ string "Redirecting..."
             NewLogin -> do
                pn <- evalCommand tm $ getNewPlayerNumber
                link <- showURL $ Noop pn
                webCommand tm pn $ newPlayer PlayerMulti { _mPlayerNumber = pn, _mPlayerName = login, _mPassword = password, _viewingGame = Nothing, _mMail = defaultMailSettings, _lastRule = Nothing}
                webCommand tm pn $ mailSettings ms pn
                seeOther link $ string "Redirecting..."
       (Left _) -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."



postLogin :: (TVar Session) -> RoutedNomyxServer Html
postLogin tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" $ loginForm Nothing
    case r of
       (Right lp) -> checkLoginPassword lp tm
       (Left _) -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."

checkLoginPassword :: LoginPass -> (TVar Session) -> RoutedNomyxServer Html
checkLoginPassword lp@(LoginPass login password) tm = do
          liftRouteT $ lift $ putStrLn $ "login:" ++ login
          liftRouteT $ lift $ putStrLn $ "password:" ++ password
          mpn <- evalCommand tm $ checkLoginWeb login password
          case mpn of
             LoginOK pn -> do
                link <- showURL $ Noop pn
                seeOther link $ string "Redirecting..."
             WrongPassword -> do
                link <- showURL $ Login
                seeOther link $ string "Redirecting..."
             NewLogin -> do
                link <- showURL $ NewPlayer lp
                seeOther link $ string "Redirecting..."

data LoginResult = LoginOK PlayerNumber | WrongPassword | NewLogin

checkLoginWeb :: PlayerName -> PlayerPassword -> StateT Multi IO LoginResult
checkLoginWeb name pwd = do
   mpn <- findPlayer name
   case mpn of
      Just pl -> do
         traceM $ "Trying name:" ++ (mPlayerName ^$ pl)
         case pwd == (mPassword ^$ pl) of
            True -> do
               traceM "password OK"
               return $ LoginOK $ mPlayerNumber ^$ pl
            False -> do
               traceM "password false"
               return WrongPassword
      Nothing -> do
         traceM "New player"
         return NewLogin


