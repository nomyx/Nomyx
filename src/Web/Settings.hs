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
import Utils
import Control.Concurrent.STM
import Data.Maybe
import Data.Text(Text)
import Text.Blaze.Internal(string)
import Data.Lens
import Multi
default (Integer, Double, Data.Text.Text)



settingsForm :: (Maybe MailSettings) -> NomyxForm MailSettings
settingsForm (Just (MailSettings {_mailTo, _mailNewRule})) = settingsForm' _mailTo _mailNewRule
settingsForm Nothing = settingsForm' "" True

settingsForm':: String -> Bool -> NomyxForm MailSettings
settingsForm' mailTo mailNewRule = pure MailSettings <*> label "Please enter your mail: " ++> inputText mailTo <++ br
                             <*> pure True --label " send mail on new input needed from you: " ++> inputCheckbox True <++ label " " <++ br
                             <*> inputCheckbox mailNewRule <++ label " I want to be notified by email when a player proposes a new rule in my game (recommended)" <++ br
                             <*> pure True --label " send mail on new output: " ++> inputCheckbox True <++ label " "
                             <*> pure True

settingsPage :: PlayerNumber -> MailSettings -> RoutedNomyxServer Html
settingsPage pn ms = do
   settingsLink <- showURL (SubmitPlayerSettings pn)
   mf <- lift $ viewForm "user" $ settingsForm (Just ms)
   mainPage (blazeForm mf settingsLink)
             "Player settings"
             "Player settings"
             False


settings :: PlayerNumber -> (TVar Session) -> RoutedNomyxServer Html
settings pn tm  = do
   pm <- evalCommand tm $ findPlayer' pn
   settingsPage pn $ mMail ^$ fromJust pm


newSettings :: PlayerNumber -> (TVar Session) -> RoutedNomyxServer Html
newSettings pn tm = do
   methodM POST
   r <- liftRouteT $ eitherForm environment "user" $ settingsForm Nothing
   link <- showURL $ Noop pn
   case r of
       Right ms -> webCommand tm pn $ mailSettings ms pn
       (Left _) -> liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
   seeOther link $ string "Redirecting..."
