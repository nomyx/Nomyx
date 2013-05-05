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
import Language.Nomyx.Expression
import Web.Routes.RouteT
import Control.Concurrent.STM
import Data.Maybe
import Data.Text(Text)
import Text.Blaze.Internal(string)
import Multi
import qualified Data.Acid.Advanced as A (query')
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

settingsPage :: PlayerNumber -> PlayerSettings -> RoutedNomyxServer Html
settingsPage pn ps = do
   settingsLink <- showURL (SubmitPlayerSettings pn)
   mf <- lift $ viewForm "user" $ settingsForm (Just ps)
   mainPage  "Player settings"
             "Player settings"
             (blazeForm mf settingsLink)
             False

settings :: PlayerNumber -> (TVar Session) -> RoutedNomyxServer Html
settings pn ts  = do
   s <- liftRouteT $ lift $ atomically $ readTVar ts
   pfd <- A.query' (acidProfileData $ _acid s) (AskProfileData pn)
   settingsPage pn $ _pPlayerSettings $ fromJust pfd

newSettings :: PlayerNumber -> (TVar Session) -> RoutedNomyxServer Html
newSettings pn tm = do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" $ settingsForm Nothing
   link <- showURL $ Noop pn
   case p of
       Right ps -> webCommand tm pn $ playerSettings ps pn
       (Left _) -> liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
   seeOther link $ string "Redirecting..."
