{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules#-}

module Web.Login where


import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map, label, br)
import Text.Blaze.Html5.Attributes hiding (dir, label)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.RouteT
import Text.Blaze.Internal
import Control.Monad.State
import Control.Concurrent.STM
import Language.Nomyx.Expression
import Utils
import Text.Reform.Happstack
import Text.Reform

import Happstack.Server
import Types
import Serialize
import Multi --TODO to remove
import Web.Common
import Web.Settings
import Web.Routes.Happstack()
import Control.Applicative
import Text.Reform.Blaze.String hiding (form)
import qualified Text.Reform.Blaze.Common as RBC
import Data.Text hiding (map, zip, concatMap)
default (Integer, Double, Data.Text.Text)


loginForm :: Maybe LoginPass -> NomyxForm LoginPass
loginForm (Just (LoginPass login _)) = loginForm' login
loginForm Nothing = loginForm' ""

loginForm' :: String -> NomyxForm LoginPass
loginForm' login = LoginPass <$> label "Please enter your login and password (if you dont have one, just invent it):" ++> br
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
   mf  <- lift $ viewForm "user" $ mailForm Nothing
   mainPage (do
                lf ! (disabled "")
                H.br >> H.br
                "New Player? Welcome!"
                (blazeForm mf link))
             "Login to Nomyx"
             "New Player"
             True

newPlayerLogin :: (TVar Multi) -> LoginPass -> RoutedNomyxServer Html
newPlayerLogin tm (LoginPass login password) = do
    methodM POST
    liftRouteT $ lift $ putStrLn $ "newPlayerLogin"
    r <- liftRouteT $ eitherForm environment "user" $ mailForm Nothing
    case r of
       (Right ms) -> do
          mpn <- execCommand tm $ checkLoginWeb login password
          case mpn of
             LoginOK pn -> do
                link <- showURL $ Noop pn
                execCommand tm $ update $ MultiMailSettings ms pn
                seeOther link $ string "Redirecting..."
             WrongPassword -> do
                link <- showURL $ Login
                seeOther link $ string "Redirecting..."
             NewLogin -> do
                pn <- execCommand tm $ getNewPlayerNumber
                link <- showURL $ Noop pn
                execCommand tm $ update $ MultiNewPlayer PlayerMulti { mPlayerNumber = pn, mPlayerName = login, mPassword = password, inGame = Nothing, mMail = defaultMailSettings}
                execCommand tm $ update $ MultiMailSettings ms pn
                seeOther link $ string "Redirecting..."
       (Left _) -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."



postLogin :: (TVar Multi) -> RoutedNomyxServer Html
postLogin tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" $ loginForm Nothing
    case r of
       (Right lp) -> checkLoginPassword lp tm
       (Left _) -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."

checkLoginPassword :: LoginPass -> (TVar Multi) -> RoutedNomyxServer Html
checkLoginPassword lp@(LoginPass login password) tm = do
          liftRouteT $ lift $ putStrLn $ "login:" ++ login
          liftRouteT $ lift $ putStrLn $ "password:" ++ password
          mpn <- execCommand tm $ checkLoginWeb login password
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
   --find that name among the list
   mpn <- findPlayer name
   case mpn of
      Just pl -> do
         say $ "Trying name:" ++ mPlayerName pl
         case pwd == mPassword pl of
            True -> do
               say "password OK"
               return $ LoginOK $ mPlayerNumber pl
            False -> do
               say "password false"
               return WrongPassword
      Nothing -> do
         say "New player"
         --add the new player to the list
         return NewLogin
         --pn <- getNewPlayerNumber
         --update $ MultiNewPlayer PlayerMulti { mPlayerNumber = pn, mPlayerName = name, mPassword = pwd, inGame = Nothing, mMail = defaultMailSettings}
        -- return (Just pn)

