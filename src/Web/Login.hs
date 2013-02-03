{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules#-}

module Web.Login where


import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map, label)
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
import Web.Routes.Happstack()
import Control.Applicative
import Text.Reform.Blaze.String hiding (form)
import Data.Text hiding (map, zip, concatMap)
default (Integer, Double, Data.Text.Text)

data LoginPass = LoginPass { login :: PlayerName,
                             password :: PlayerPassword}
                             deriving (Show, Eq)

loginForm :: NomyxForm LoginPass
loginForm = pure LoginPass <*> label "Login: " ++> (inputText "") <*> label "    Password: " ++> inputPassword <++ label " "

loginPage :: RoutedNomyxServer Html
loginPage = do
   link <- showURL PostLogin
   lf  <- lift $ viewForm "user" loginForm
   mainPage (blazeForm lf link)
             "Login to Nomyx"
             "Login to Nomyx"
             True


postLogin :: (TVar Multi) -> RoutedNomyxServer Html
postLogin tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" loginForm
    case r of
       (Right (LoginPass login password)) -> do
          liftRouteT $ lift $ putStrLn $ "login:" ++ login
          liftRouteT $ lift $ putStrLn $ "password:" ++ password
          mpn <- execCommand tm $ newPlayerWeb login password
          case mpn of
             Just pn -> do
                link <- showURL $ Noop pn
                seeOther link $ string "Redirecting..."
             _ -> error "cannot login"
       (Left _) -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."



newPlayerWeb :: PlayerName -> PlayerPassword -> StateT Multi IO (Maybe PlayerNumber)
newPlayerWeb name pwd = do
   --find that name among the list
   mpn <- findPlayer name
   case mpn of
      Just pl -> do
         say $ "Trying name:" ++ mPlayerName pl
         case pwd == mPassword pl of
            True -> do
               say "password OK"
               return $ Just $ mPlayerNumber pl
            False -> do
               say "password false"
               return Nothing
      Nothing -> do
         say "New player"
         --add the new player to the list
         pn <- getNewPlayerNumber
         update $ MultiNewPlayer PlayerMulti { mPlayerNumber = pn, mPlayerName = name, mPassword = pwd, inGame = Nothing, mMail = defaultMailSettings}
         return (Just pn)

