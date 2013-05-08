{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


module Web.Settings where

import Text.Blaze.Html5 hiding (map, label, br)

import Prelude hiding (div)
import Text.Reform
import Text.Reform.Blaze.String hiding (form)
import Text.Reform.Happstack()
import Control.Applicative
import Types
import Happstack.Server
import Text.Reform.Happstack
import Web.Common
import Control.Monad.State
import Web.Routes.RouteT
import Control.Concurrent.STM
import Data.Maybe
import Data.Text(Text)
import Text.Blaze.Internal(string)
import Multi
import Utils
default (Integer, Double, Data.Text.Text)



settingsForm :: (Maybe PlayerSettings) -> NomyxForm PlayerSettings
settingsForm (Just prof) = settingsForm' (_pPlayerName prof) (_mailTo prof) (_mailNewRule prof)
settingsForm Nothing = settingsForm' "" "" True

settingsForm':: String -> String -> Bool -> NomyxForm PlayerSettings
settingsForm' name mailTo mailNewRule = pure Types.PlayerSettings
   <*> label "Player Name: " ++> (inputText name) <++ br
   <*> label "Please enter your mail: " ++> inputText mailTo <++ br
   <*> pure True --label " send mail on new input needed from you: " ++> inputCheckbox True <++ label " " <++ br
   <*> inputCheckbox mailNewRule <++ label " I want to be notified by email when a player proposes a new rule in my game (recommended)" <++ br
   <*> pure True --label " send mail on new output: " ++> inputCheckbox True <++ label " "
   <*> pure True

settingsPage :: PlayerSettings -> RoutedNomyxServer Html
settingsPage ps = do
   settingsLink <- showURL SubmitPlayerSettings
   mf <- lift $ viewForm "user" $ settingsForm (Just ps)
   mainPage  "Player settings"
             "Player settings"
             (blazeForm mf settingsLink)
             False

settings :: (TVar Session) -> RoutedNomyxServer Html
settings ts  = do
   s <- liftRouteT $ lift $ atomically $ readTVar ts
   pn <- getPlayerNumber ts
   pfd <- getProfile pn s
   settingsPage $ _pPlayerSettings $ fromJust pfd

newSettings :: (TVar Session) -> RoutedNomyxServer Html
newSettings ts = do
   methodM POST
   pn <- getPlayerNumber ts
   p <- liftRouteT $ eitherForm environment "user" $ settingsForm Nothing
   link <- showURL MainPage
   case p of
       Right ps -> webCommand ts $ playerSettings ps pn
       (Left _) -> liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
   seeOther link $ string "Redirecting..."
