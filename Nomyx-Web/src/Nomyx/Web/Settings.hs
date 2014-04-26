{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Nomyx.Web.Settings where

import Text.Blaze.Html5 hiding (map, label, br)

import Prelude hiding (div)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.Blaze.String as RB hiding (form)
import Text.Blaze.Html5.Attributes as A hiding (dir, label)
import qualified Text.Blaze.Html5 as H
import Happstack.Server
import Web.Routes.RouteT
import Control.Applicative
import Control.Monad.State
import Control.Concurrent.STM
import Safe
import Data.Text(Text)
import Data.Version (showVersion)
import Data.Maybe
import Data.String
import System.FilePath
import Paths_Nomyx_Web as PNW
import Language.Nomyx
import Nomyx.Web.Common
import Nomyx.Web.Help as Help
import Nomyx.Core.Profile
import Nomyx.Core.Utils
import Nomyx.Core.Types as Types
import Nomyx.Core.Session as S

default (Integer, Double, Data.Text.Text)

playerSettingsForm :: Maybe PlayerSettings -> [PlayerName] -> [String] -> NomyxForm PlayerSettings
playerSettingsForm (Just prof) ns emails = playerSettingsForm' (_pPlayerName prof) (_mail prof) (_mailNewRule prof) ns emails
playerSettingsForm Nothing ns emails = playerSettingsForm' "" "" True ns emails

playerSettingsForm':: String -> String -> Bool -> [PlayerName] -> [String] -> NomyxForm PlayerSettings
playerSettingsForm' name mailTo mailNewRule names emails = pure Types.PlayerSettings
   <*> errorList ++> label "Player Name: " ++> (RB.inputText name) `transformEither` uniqueName names
                                                                   `transformEither` fieldRequired PlayerNameRequired
                                                                   `transformEither` maxLength 15 <++ br
   <*> errorList ++> label "Please enter your mail: " ++> (RB.inputText mailTo) `transformEither` uniqueEmail emails <++ br
   <*> pure True
   <*> RB.inputCheckbox mailNewRule <++ label " I want to be notified by email when a player proposes a new rule in my game (recommended)" <++ br
   <*> pure True
   <*> pure True

uniqueName :: [String] -> String -> Either NomyxError String
uniqueName names name = if name `elem` names then Left UniqueName else Right name

uniqueEmail :: [String] -> String -> Either NomyxError String
uniqueEmail names name = if name `elem` names then Left UniqueEmail else Right name

maxLength :: Int -> String -> Either NomyxError String
maxLength l name = if length name > l then Left (FieldTooLong l) else Right name

settingsPage :: PlayerSettings -> [PlayerName] -> [String] -> RoutedNomyxServer Html
settingsPage ps names emails = do
   settingsLink <- showURL SubmitPlayerSettings
   mf <- lift $ viewForm "user" $ playerSettingsForm (Just ps) names emails
   mainPage  "Player settings"
             "Player settings"
             (blazeForm mf settingsLink)
             False
             True

playerSettings :: TVar Session -> RoutedNomyxServer Response
playerSettings ts  = toResponse <$> do
   pn <- fromJust <$> getPlayerNumber ts
   pfd <- getProfile' ts pn
   names <- liftIO $ forbiddenNames ts pn
   emails <- liftIO $ forbiddenEmails ts pn
   settingsPage (_pPlayerSettings $ fromJustNote "playerSettings" pfd) names emails

newPlayerSettings :: TVar Session -> RoutedNomyxServer Response
newPlayerSettings ts = toResponse <$> do
   methodM POST
   pn <- fromJust <$> getPlayerNumber ts
   names <- liftIO $ forbiddenNames ts pn
   emails <- liftIO $ forbiddenEmails ts pn
   p <- liftRouteT $ eitherForm environment "user" $ playerSettingsForm Nothing names emails
   case p of
      Right ps -> do
         webCommand ts $ S.playerSettings ps pn
         link <- showURL MainPage
         seeOther link $ fromString "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitPlayerSettings
         mainPage  "Player settings" "Player settings" (blazeForm errorForm settingsLink) False True

forbiddenNames :: TVar Session -> PlayerNumber -> IO [PlayerName]
forbiddenNames ts pn = liftIO $ do
   session <- atomically $ readTVar ts
   pfs <- getAllProfiles session
   let filteredPfs = filter ((/= pn) . _pPlayerNumber) pfs
   return $ (_pPlayerName . _pPlayerSettings) <$> filteredPfs

forbiddenEmails :: TVar Session -> PlayerNumber -> IO [PlayerName]
forbiddenEmails ts pn = liftIO $ do
   session <- atomically $ readTVar ts
   pfs <- getAllProfiles session
   let filteredPfs = filter ((/= pn) . _pPlayerNumber) pfs
   return $ filter (not . null) $ (_mail . _pPlayerSettings) <$> filteredPfs


advanced :: TVar Session -> RoutedNomyxServer Response
advanced ts = toResponse <$> do
   session <- liftIO $ atomically $ readTVar ts
   pn <- fromJust <$> getPlayerNumber ts
   pfd <- getProfile session pn
   pfds <- liftIO $ getAllProfiles session
   session <- liftIO $ atomically $ readTVar ts
   let prof = fromJustNote "advanced" pfd
   page <- advancedPage (_pLastUpload prof)
                        (_pIsAdmin prof)
                        (_mSettings $ _multi session)
                        pfds
   mainPage "Advanced" "Advanced" page False True


advancedPage :: LastUpload -> Bool -> Settings -> [ProfileData] -> RoutedNomyxServer Html
advancedPage mlu isAdmin settings pfds = do
   uploadLink <- showURL Upload
   submitAdminPass <- showURL SubmitAdminPass
   submitSettings <- showURL SubmitSettings
   getSaveFile <- showURL SaveFilePage
   up  <- lift $ viewForm "user" uploadForm  --TODO add the file name (missing Reform feature)
   ap  <- lift $ viewForm "user" adminPassForm
   set <- lift $ viewForm "user" $ settingsForm (_sendMails settings)
   let uploadExample =  pathSeparator : testDir </> "SimpleModule.hs"
   ok $ do
      p $ do
         fromString "Version:"
         pre $ fromString $ "Nomyx " ++ showVersion PNW.version ++ "\n"
      hr
      p $ do
         pre $ fromString Help.getSaveFile
         H.a "get save file" ! (href $ toValue getSaveFile)
      H.br
      hr
      p $ do
         pre $ fromString Help.upload
         H.a "example upload file" ! (href $ toValue uploadExample)
         H.br >> H.br
         "Upload new rules file:" >> H.br
         blazeForm up uploadLink
         case mlu of
            UploadFailure (_, error) -> do
               h5 "Error in submitted file: "
               pre $ fromString error
            UploadSuccess -> h5 "File uploaded successfully!"
            NoUpload -> p ""
      hr
      p $ do
         h5 "Enter admin password to get admin rights (necessary to create a new game):"
         blazeForm ap submitAdminPass
         when isAdmin $ h5 "You are admin"
      when isAdmin $ do
         hr
         p $ do
            h5 "Send mails:"
            blazeForm set submitSettings
            h5 $ fromString $ if _sendMails settings then "mails will be sent " else "mails will NOT be sent "
         hr
         p $ do
            h5 "Players:"
            table ! A.class_ "table" $ do
               thead $ do
                  td ! A.class_ "td" $ "#"
                  td ! A.class_ "td" $ "Name"
                  td ! A.class_ "td" $ "mail"
                  td ! A.class_ "td" $ "send mails"
                  td ! A.class_ "td" $ "viewing game"
                  td ! A.class_ "td" $ "last rule"
                  td ! A.class_ "td" $ "last upload"
                  td ! A.class_ "td" $ "is admin"
                  td ! A.class_ "td" $ "play as"
               mapM_ viewProfile pfds


viewProfile :: ProfileData -> Html
viewProfile (ProfileData pn (Types.PlayerSettings playerName mail _ mailNewRule _ _) viewingGame lastRule lastUpload isAdmin) =
   tr $ do
      td ! A.class_ "td" $ fromString $ show pn
      td ! A.class_ "td" $ fromString playerName
      td ! A.class_ "td" $ fromString mail
      td ! A.class_ "td" $ fromString $ show mailNewRule
      td ! A.class_ "td" $ fromString $ show viewingGame
      td ! A.class_ "td" $ fromString $ show lastRule
      td ! A.class_ "td" $ fromString $ show lastUpload
      td ! A.class_ "td" $ fromString $ show isAdmin


adminPassForm :: NomyxForm String
adminPassForm = RB.inputText ""

settingsForm :: Bool -> NomyxForm Bool
settingsForm sendMails = label "Send mails: " ++> RB.inputCheckbox sendMails

newSettings :: TVar Session -> RoutedNomyxServer Response
newSettings ts = toResponse <$> do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" $ settingsForm False
   case p of
      Right ps -> do
         webCommand ts $ globalSettings ps
         link <- showURL Advanced
         seeOther link "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitSettings
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True


uploadForm :: NomyxForm (FilePath, FilePath, ContentType)
uploadForm = RB.inputFile

newUpload :: TVar Session -> RoutedNomyxServer Response
newUpload ts = toResponse <$> do
    methodM POST
    pn <- fromJust <$> getPlayerNumber ts
    r <- liftRouteT $ eitherForm environment "user" uploadForm
    link <- showURL Advanced
    (Types.Session sh _ _) <- liftIO $ readTVarIO ts
    case r of
       (Right (temp,name,_)) -> webCommand ts $ void $ S.inputUpload pn temp name sh
       (Left _) -> liftIO $ putStrLn "cannot retrieve form data"
    seeOther link "Redirecting..."


newAdminPass :: TVar Session -> RoutedNomyxServer Response
newAdminPass ts = toResponse <$> do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" adminPassForm
   pn <- fromJust <$> getPlayerNumber ts
   case p of
      Right ps -> do
         webCommand ts $ adminPass ps pn
         link <- showURL Advanced
         seeOther link "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitAdminPass
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True

saveFilePage :: TVar Session -> RoutedNomyxServer Response
saveFilePage ts = toResponse <$> do
   session <- liftIO $ atomically $ readTVar ts
   liftIO $ makeTar (_saveDir $ _mSettings $ _multi session)
   seeOther (pathSeparator : tarFile) "Redirecting..."
