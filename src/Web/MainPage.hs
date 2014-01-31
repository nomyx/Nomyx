
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables, NamedFieldPuns, Rank2Types, DoAndIfThenElse, StandaloneDeriving, OverloadedStrings,
   ExtendedDefaultRules, RecordWildCards#-}

module Web.MainPage (launchWebServer) where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.Site
import Web.Routes.PathInfo
import Web.Routes.Happstack
import Web.Routes.RouteT
import Text.Blaze.Internal
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Control.Concurrent.STM
import Language.Nomyx
import Language.Nomyx.Engine
import Happstack.Server as HS
import System.FilePath
import qualified Web.Help as Help
import Types as T
import Web.Game
import Web.Common as W
import Web.Settings
import Web.NewGame
import Web.Login
import Data.List
import Utils
import Data.Text(Text, pack)
import qualified Language.Nomyx.Engine as G
import Happstack.Auth
import Safe
import Paths_Nomyx_Language
import Data.Maybe
default (Integer, Double, Data.Text.Text)

viewMulti :: PlayerNumber -> FilePath -> Session -> RoutedNomyxServer Html
viewMulti pn saveDir s = do
   pfd <- getProfile s pn
   let isAdmin = _pIsAdmin $ fromJustNote "viewMulti" pfd
   gns <- viewGamesTab (map G._game $ _games $ _multi s) isAdmin saveDir pn
   mgn <- liftRouteT $ lift $ getPlayersGame pn s
   vg <- case mgn of
      Just g -> viewGame (G._game g) pn (_pLastRule $ fromJustNote "viewMulti" pfd) isAdmin
      Nothing -> ok $ h3 "Not viewing any game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ vg

viewGamesTab :: [Game] -> Bool -> FilePath -> PlayerNumber -> RoutedNomyxServer Html
viewGamesTab gs isAdmin saveDir pn = do
   gns <- mapM (\g -> viewGameName isAdmin pn (isSimulated g gs) g) gs
   newGameLink  <- showURL NewGame
   settingsLink <- showURL W.PlayerSettings
   advLink      <- showURL Advanced
   logoutURL    <- showURL (U_AuthProfile $ AuthURL A_Logout)
   fmods <- liftIO $ getUploadedModules saveDir
   ok $ do
      h3 "Main menu" >> br
      "Active games:" >> br
      table $ do
         case gs of
            [] -> tr $ td "No Games"
            _ ->  sequence_ gns
      when isAdmin $ H.a "Create a new game" ! (href $ toValue newGameLink) >> br
      br >> "Help files:" >> br
      H.a "Rules examples"    ! (href $ "/html/Language-Nomyx-Examples.html") >> br
      H.a "Nomyx language"    ! (href $ "/html/Language-Nomyx.html") >> br
      when (fmods /= []) $ do
         br >> "Uploaded files:" >> br
         mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : uploadDir </> f)) >> br) (sort fmods)
      br >> "Settings:" >> br
      H.a "Player settings" ! (href $ toValue settingsLink) >> br
      H.a "Advanced"        ! (href $ toValue advLink) >> br
      H.a "Logout"          ! (href $ toValue logoutURL) >> br


viewGameName :: Bool -> PlayerNumber -> Bool -> Game -> RoutedNomyxServer Html
viewGameName isAdmin pn isForked g = do
   let isGameAdmin = isAdmin || isOwnerOfGame g pn
   let gn = _gameName g
   let isForkable = isNothing $ _simu g
   main  <- showURL (W.MainPage)
   join  <- showURL (W.JoinGame gn)
   leave <- showURL (W.LeaveGame gn)
   view  <- showURL (W.ViewGame gn)
   del   <- showURL (W.DelGame gn)
   fork  <- showURL (W.ForkGame gn)
   if (isGameAdmin || (isNothing $ _simu g)) then
    ok $ tr $ do
      let cancel = H.a "Cancel" ! (href $ toValue main) ! A.class_ "modalButton"
      td ! A.id "gameName" $ string $ (gn ++ "   ")
      td $ H.a "View"  ! (href $ toValue view) ! (A.title $ toValue Help.view)
      td $ H.a "Join"  ! (href $ toValue $ "#openModalJoin" ++ gn) ! (A.title $ toValue Help.join)
      td $ H.a "Leave" ! (href $ toValue $ "#openModalLeave" ++ gn)
      when isGameAdmin $ td $ H.a "Del"   ! (href $ toValue del)
      when (isForkable && not isForked) $ td $ H.a "Fork"  ! (href $ toValue $ "#openModalFork" ++ gn)
      div ! A.id (toValue $ "openModalJoin" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 "Joining the game. Please register in the Agora (see the link) and introduce yourself to the other players! \n \
                If you do not wich to play, you can just view the game."
            cancel
            H.a "Join" ! (href $ toValue join) ! A.class_ "modalButton" ! (A.title $ toValue Help.join)
      div ! A.id (toValue $ "openModalLeave" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 "Do you really want to leave? You will loose your assets in the game (for example, your bank account)."
            cancel
            H.a "Leave" ! (href $ toValue leave) ! A.class_ "modalButton"
      div ! A.id (toValue $ "openModalFork" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 $ string $ "Fork game \"" ++ gn ++ "\"? This will create a new game based on the previous one. You will be able to test \n \
               your new rules independently of the original game. The new game is private: you will be alone. Please delete it when finished."
            cancel
            H.a "Fork" ! (href $ toValue fork) ! A.class_ "modalButton"
   else ok ""

nomyxPage :: (TVar Session) -> RoutedNomyxServer Response
nomyxPage ts = do
   pn <- getPlayerNumber ts
   s <- liftIO $ atomically $ readTVar ts
   let saveDir = _saveDir $ _mSettings $ _multi s
   name <- liftIO $ Utils.getPlayerName pn s
   pn <- getPlayerNumber ts
   m <- viewMulti pn saveDir s
   mainPage' "Welcome to Nomyx!"
            (string $ "Welcome to Nomyx, " ++ name ++ "! ")
            (H.div ! A.id "multi" $ m)
            False

nomyxSite :: (TVar Session) -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite tm = setDefault HomePage $ mkSitePI (runRouteT $ routedNomyxCommands tm)

routedNomyxCommands :: (TVar Session) -> PlayerCommand -> RoutedNomyxServer Response
routedNomyxCommands ts (U_AuthProfile auth)  = authenticate      ts auth
routedNomyxCommands ts PostAuth              = postAuthenticate  ts
routedNomyxCommands ts HomePage              = homePage          ts
routedNomyxCommands ts MainPage              = nomyxPage         ts
routedNomyxCommands ts (W.JoinGame game)     = joinGame          ts game
routedNomyxCommands ts (W.LeaveGame game)    = leaveGame         ts game
routedNomyxCommands ts (ViewGame game)       = viewGamePlayer    ts game
routedNomyxCommands ts (DelGame game)        = delGame           ts game
routedNomyxCommands ts (ForkGame game)       = forkGame          ts game
routedNomyxCommands ts (NewRule game)        = newRule           ts game
routedNomyxCommands _  NewGame               = newGamePage
routedNomyxCommands ts SubmitNewGame         = newGamePost       ts
routedNomyxCommands ts (DoInput en game)     = newInput          ts en game
routedNomyxCommands ts Upload                = newUpload         ts
routedNomyxCommands ts W.PlayerSettings      = playerSettings    ts
routedNomyxCommands ts SubmitPlayerSettings  = newPlayerSettings ts
routedNomyxCommands ts Advanced              = advanced          ts
routedNomyxCommands ts (SubmitPlayAs game)   = newPlayAs         ts game
routedNomyxCommands ts SubmitAdminPass       = newAdminPass      ts
routedNomyxCommands ts SubmitSettings        = newSettings       ts

launchWebServer :: (TVar Session) -> Network -> IO ()
launchWebServer tm net = do
   putStrLn $ "Starting web server...\nTo connect, drive your browser to \"" ++ nomyxURL net ++ "/Nomyx\""
   simpleHTTP nullConf {HS.port = T._port net} $ server tm net

--serving Nomyx web page as well as data from this package and the language library package
server :: (TVar Session) -> Network -> ServerPartT IO Response
server ts net = do
  s <- liftIO $ atomically $ readTVar ts
  let set = _mSettings $ _multi s
  docdir <- liftIO $ getDocDir
  mconcat [
    serveDirectory EnableBrowsing [] (_saveDir set),
    serveDirectory EnableBrowsing [] docdir,
    serveDirectory EnableBrowsing [] (_dataDir set),
    serveDirectory EnableBrowsing [] (_sourceDir set),
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack (nomyxURL net)) "/Nomyx" (nomyxSite ts)
       return $ toResponse html]


getDocDir :: IO FilePath
getDocDir = do
   datadir <- getDataDir
   let (as, _:bs) = break (== "share") $ splitDirectories datadir
   return $ joinPath $ as ++ ["share", "doc"] ++ bs

isOwnerOfGame :: Game -> PlayerNumber -> Bool
isOwnerOfGame g pn = case _simu g of
   Just (Simulation {_ownedBy = ob}) -> ob == pn
   Nothing -> False

