{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Nomyx.Web.Settings where

import           Control.Applicative
import           Control.Monad.State
import           Data.Maybe
import           Data.String
import           Data.Text                   (Text)
import           Data.Version                (showVersion)
import           Happstack.Server
import           Nomyx.Core.Profile
import           Nomyx.Core.Session          as S
import           Nomyx.Core.Types            as Types
import           Nomyx.Core.Utils
import           Nomyx.Web.Common
import           Nomyx.Web.Help              as Help
import           Nomyx.Web.Types
import           Paths_Nomyx_Web             as PNW
import           Prelude                     hiding (div)
import           Safe
import           System.FilePath
import           Text.Blaze.Html5            hiding (br, label, map)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A hiding (dir, label)
import           Text.Reform
import           Text.Reform.Blaze.String    as RB hiding (form)
import           Text.Reform.Happstack
import           Web.Routes.RouteT

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
   let sendMail = _sendMails $ _mailSettings settings
   ap  <- liftRouteT $ lift $ viewForm "user" adminPassForm
   set <- liftRouteT $ lift $ viewForm "user" $ settingsForm sendMail 
   let uploadExample =  pathSeparator : testDir </> "SimpleModule.hs"
   ok $ do
      
      
      --p $ do
      --   fromString "Version:"
      --   pre $ fromString $ "Nomyx " ++ showVersion PNW.version ++ "\n"
      --hr
      --p $ do
      --   pre $ fromString Help.getSaveFile
      --   H.a "get save file" ! (href $ toValue $ showRelURL SaveFilePage)
      --H.br
      hr
      p $ do
         h5 "Enter admin password to get admin rights (necessary to create public games):"
         blazeForm ap $ showRelURL SubmitAdminPass
         when isAdmin $ h5 "You are admin"
      when isAdmin $ do
         hr
         p $ do
            h5 "Send mails:"
            blazeForm set $ showRelURL SubmitSettings
            h5 $ fromString $ if sendMail then "mails will be sent " else "mails will NOT be sent "
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
viewProfile (ProfileData pn (Types.PlayerSettings playerName mail _ mailSubmitRule _ _) lastRule lastUpload isAdmin _) =
   tr $ do
      td ! A.class_ "td" $ fromString $ show pn
      td ! A.class_ "td" $ fromString playerName
      td ! A.class_ "td" $ fromString $ show mail
      td ! A.class_ "td" $ fromString $ show mailSubmitRule
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
         seeOther (showRelURL Advanced) "Redirecting..."
      (Left errorForm) -> mainPage  "Admin settings" "Admin settings" (blazeForm errorForm $ showRelURL SubmitSettings) False True

newAdminPass :: RoutedNomyxServer Response
newAdminPass = toResponse <$> do
   methodM POST
   p <- liftRouteT $ lift $ eitherForm environment "user" adminPassForm
   pn <- fromJust <$> getPlayerNumber
   case p of
      Right ps -> do
         webCommand $ adminPass ps pn
         seeOther (showRelURL Advanced) "Redirecting..."
      (Left errorForm) -> mainPage  "Admin settings" "Admin settings" (blazeForm errorForm $ showRelURL SubmitAdminPass) False True

saveFilePage :: RoutedNomyxServer Response
saveFilePage = toResponse <$> do
   session <- getSession
   liftIO $ makeTar (_saveDir $ _mSettings $ _multi session)
   seeOther (pathSeparator : tarFile) "Redirecting..."
