{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


module Web.Settings where

import Text.Blaze.Html5 hiding (map, label, br)

import Prelude hiding (div)
import Text.Reform
import Text.Reform.Blaze.String as RB hiding (form)
import Text.Reform.Happstack()
import Text.Blaze.Html5.Attributes hiding (dir, label)
import qualified Text.Blaze.Html5 as H
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
default (Integer, Double, Data.Text.Text)

settingsForm :: (Maybe PlayerSettings) -> [PlayerName] -> [String] -> NomyxForm PlayerSettings
settingsForm (Just prof) ns emails = settingsForm' (_pPlayerName prof) (_mail prof) (_mailNewRule prof) ns emails
settingsForm Nothing ns emails = settingsForm' "" "" True ns emails

settingsForm':: String -> String -> Bool -> [PlayerName] -> [String] -> NomyxForm PlayerSettings
settingsForm' name mailTo mailNewRule names emails = pure Types.PlayerSettings
   <*> errorList ++> label "Player Name: " ++> (RB.inputText name) `transformEither` (uniqueName names) `transformEither` (fieldRequired PlayerNameRequired) <++ br
   <*> errorList ++> label "Please enter your mail: " ++> (RB.inputText mailTo) `transformEither` (uniqueEmail emails) <++ br
   <*> pure True
   <*> RB.inputCheckbox mailNewRule <++ label " I want to be notified by email when a player proposes a new rule in my game (recommended)" <++ br
   <*> pure True
   <*> pure True

readPlayAs :: NomyxForm Bool -> NomyxForm String -> NomyxForm (Maybe PlayerNumber)
readPlayAs = liftA2 f where
   f b s = if b then (Just $ read s) else Nothing

uniqueName :: [String] -> String -> Either NomyxError String
uniqueName names name = case name `elem` names of
   True  -> Left UniqueName
   False -> Right name

uniqueEmail :: [String] -> String -> Either NomyxError String
uniqueEmail names name = case name `elem` names of
   True  -> Left UniqueEmail
   False -> Right name

settingsPage :: PlayerSettings -> [PlayerName] -> [String] -> RoutedNomyxServer Html
settingsPage ps names emails = do
   settingsLink <- showURL SubmitPlayerSettings
   mf <- lift $ viewForm "user" $ settingsForm (Just ps) names emails
   mainPage  "Player settings"
             "Player settings"
             (blazeForm mf settingsLink)
             False

settings :: (TVar Session) -> RoutedNomyxServer Html
settings ts  = do
   pn <- getPlayerNumber ts
   pfd <- getProfile' ts pn
   names <- liftIO $ forbiddenNames ts pn
   emails <- liftIO $ forbiddenEmails ts pn
   settingsPage (_pPlayerSettings $ fromJust pfd) names emails

newSettings :: (TVar Session) -> RoutedNomyxServer Html
newSettings ts = do
   methodM POST
   pn <- getPlayerNumber ts
   names <- liftIO $ forbiddenNames ts pn
   emails <- liftIO $ forbiddenEmails ts pn
   p <- liftRouteT $ eitherForm environment "user" $ settingsForm Nothing names emails
   case p of
      Right ps -> do
         webCommand ts $ playerSettings ps pn
         link <- showURL MainPage
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitPlayerSettings
         mainPage  "Player settings" "Player settings" (blazeForm errorForm settingsLink) False

forbiddenNames :: (TVar Session) -> PlayerNumber -> IO [PlayerName]
forbiddenNames ts pn = liftIO $ do
   session <- atomically $ readTVar ts
   pfs <- getAllProfiles session
   let filteredPfs = filter ((/= pn) . _pPlayerNumber) pfs
   return $ (_pPlayerName . _pPlayerSettings) <$> filteredPfs

forbiddenEmails :: (TVar Session) -> PlayerNumber -> IO [PlayerName]
forbiddenEmails ts pn = liftIO $ do
   session <- atomically $ readTVar ts
   pfs <- getAllProfiles session
   let filteredPfs = filter ((/= pn) . _pPlayerNumber) pfs
   return $ filter (not . null) $ (_mail . _pPlayerSettings) <$> filteredPfs


advanced :: RoutedNomyxServer Html
advanced = mainPage  "Advanced"
             "Advanced"
             (H.a "get save file"    ! (href $ "/Nomyx.save") >> H.br)
             False

adminForm :: [PlayerNumber] -> NomyxForm Admin
adminForm pns = pure (Admin . PlayAs)
   <*> readPlayAs (label "Play as: " ++> RB.inputCheckbox False) (RB.inputText "")

adminPage :: (TVar Session) -> RoutedNomyxServer Html
adminPage ts = do
   pn <- getPlayerNumber ts
   pfd <- getProfile' ts pn
   settingsLink <- showURL SubmitAdminSettings
   mf <- lift $ viewForm "user" $ adminForm []
   mainPage  "Admin settings"
             "Admin settings"
             (blazeForm mf settingsLink)
             False


newAdminSettings :: (TVar Session) -> RoutedNomyxServer Html
newAdminSettings ts = do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" $ adminForm []
   pn <- getPlayerNumber ts
   case p of
      Right ps -> do
         webCommand ts $ adminSettings ps pn
         link <- showURL MainPage
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitAdminSettings
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False
