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
import Language.Nomyx
import Debug.Trace (trace)
default (Integer, Double, Data.Text.Text)



settingsForm :: (Maybe PlayerSettings) -> [PlayerName]-> NomyxForm PlayerSettings
settingsForm (Just prof) ns = settingsForm' (_pPlayerName prof) (_mailTo prof) (_mailNewRule prof) ns
settingsForm Nothing ns = settingsForm' "" "" True ns

settingsForm':: String -> String -> Bool -> [PlayerName] -> NomyxForm PlayerSettings
settingsForm' name mailTo mailNewRule names = pure Types.PlayerSettings
   <*> errorList ++> label "Player Name: " ++> (inputText name) `transformEither` (uniqueName names) `transformEither` (fieldRequired PlayerNameRequired) <++ br
   <*> label "Please enter your mail: " ++> inputText mailTo <++ br
   <*> pure True --label " send mail on new input needed from you: " ++> inputCheckbox True <++ label " " <++ br
   <*> inputCheckbox mailNewRule <++ label " I want to be notified by email when a player proposes a new rule in my game (recommended)" <++ br
   <*> pure True --label " send mail on new output: " ++> inputCheckbox True <++ label " "
   <*> pure True

uniqueName :: [String] -> String -> Either NomyxError String
uniqueName names name = case trace (name ++ (show names) ++ (show (name `elem` names))) $ (name `elem` names) of
   True -> trace "left" $ Left UniquePlayerName
   False ->trace "right" $  Right name


settingsPage :: PlayerSettings-> [PlayerName]  -> RoutedNomyxServer Html
settingsPage ps ns = do
   settingsLink <- showURL SubmitPlayerSettings
   mf <- lift $ viewForm "user" $ settingsForm (Just ps) ns
   mainPage  "Player settings"
             "Player settings"
             (blazeForm mf settingsLink)
             False

settings :: (TVar Session) -> RoutedNomyxServer Html
settings ts  = do
   s <- liftIO $ atomically $ readTVar ts
   pn <- getPlayerNumber ts
   pfd <- getProfile pn s
   pfs <- liftIO $ getAllProfiles s
   let pfs' = filter (\a -> (_pPlayerNumber a /= pn)) pfs
   settingsPage (_pPlayerSettings $ fromJust pfd) ((_pPlayerName . _pPlayerSettings) <$> pfs')

newSettings :: (TVar Session) -> RoutedNomyxServer Html
newSettings ts = do
   methodM POST
   pn <- getPlayerNumber ts
   p <- liftRouteT $ eitherForm environment "user" $ settingsForm Nothing []
   link <- showURL MainPage
   settingsLink <- showURL SubmitPlayerSettings
   case p of
      Right ps -> do
         webCommand ts $ playerSettings ps pn
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> mainPage  "Player settings" "Player settings" (blazeForm errorForm settingsLink) False
