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
import Nomyx.Web.Types
import Nomyx.Core.Profile
import Nomyx.Core.Utils
import Nomyx.Core.Types as Types
import Nomyx.Core.Session as S

default (Integer, Double, Data.Text.Text)

advanced :: RoutedNomyxServer Response
advanced = toResponse <$> do
   pn <- fromJust <$> getPlayerNumber
   pfd <- getProfile' pn
   session <- getSession
   pfds <- liftIO $ getAllProfiles session
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
   up  <- liftRouteT $ lift $ viewForm "user" uploadForm  --TODO add the file name (missing Reform feature)
   ap  <- liftRouteT $ lift $ viewForm "user" adminPassForm
   set <- liftRouteT $ lift $ viewForm "user" $ settingsForm (_sendMails settings)
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
         h5 "Enter admin password to get admin rights (necessary to create public games):"
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
                  td ! A.class_ "td" $ "last rule"
                  td ! A.class_ "td" $ "last upload"
                  td ! A.class_ "td" $ "is admin"
                  td ! A.class_ "td" $ "play as"
               mapM_ viewProfile pfds


viewProfile :: ProfileData -> Html
viewProfile (ProfileData pn (Types.PlayerSettings playerName mail _ mailNewRule _ _) lastRule lastUpload isAdmin) =
   tr $ do
      td ! A.class_ "td" $ fromString $ show pn
      td ! A.class_ "td" $ fromString playerName
      td ! A.class_ "td" $ fromString $ show mail
      td ! A.class_ "td" $ fromString $ show mailNewRule
      td ! A.class_ "td" $ fromString $ show lastRule
      td ! A.class_ "td" $ fromString $ show lastUpload
      td ! A.class_ "td" $ fromString $ show isAdmin


adminPassForm :: NomyxForm String
adminPassForm = RB.inputText ""

settingsForm :: Bool -> NomyxForm Bool
settingsForm sendMails = label "Send mails: " ++> RB.inputCheckbox sendMails

newSettings :: RoutedNomyxServer Response
newSettings = toResponse <$> do
   methodM POST
   p <- liftRouteT $ lift $ eitherForm environment "user" $ settingsForm False
   case p of
      Right ps -> do
         webCommand $ globalSettings ps
         link <- showURL Advanced
         seeOther link "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitSettings
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True


uploadForm :: NomyxForm (FilePath, FilePath, ContentType)
uploadForm = RB.inputFile

newUpload :: RoutedNomyxServer Response
newUpload = toResponse <$> do
    methodM POST
    pn <- fromJust <$> getPlayerNumber
    r <- liftRouteT $ lift $ eitherForm environment "user" uploadForm
    link <- showURL Advanced
    s <- getSession
    case r of
       (Right (temp,name,_)) -> webCommand $ void $ S.inputUpload pn temp name (_sh s)
       (Left _) -> liftIO $ putStrLn "cannot retrieve form data"
    seeOther link "Redirecting..."


newAdminPass :: RoutedNomyxServer Response
newAdminPass = toResponse <$> do
   methodM POST
   p <- liftRouteT $ lift $ eitherForm environment "user" adminPassForm
   pn <- fromJust <$> getPlayerNumber
   case p of
      Right ps -> do
         webCommand $ adminPass ps pn
         link <- showURL Advanced
         seeOther link "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitAdminPass
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True

saveFilePage :: RoutedNomyxServer Response
saveFilePage = toResponse <$> do
   session <- getSession
   liftIO $ makeTar (_saveDir $ _mSettings $ _multi session)
   seeOther (pathSeparator : tarFile) "Redirecting..."
