
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
import Happstack.Server as HS
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Web.Help as Help
import Types as T
import Web.Game
import Web.Common as W
import Web.Settings
import Web.NewGame
import Web.Login
import Utils
import Data.Text(Text, pack)
import qualified Language.Nomyx.Game as G
import Happstack.Auth
import Safe

default (Integer, Double, Data.Text.Text)


viewMulti :: PlayerNumber -> PlayerNumber -> FilePath -> Session -> RoutedNomyxServer Html
viewMulti pn playAs dataDir s = do
   pfd <- getProfile s pn
   let isAdmin = _isAdmin $ _pAdmin $ fromJustNote "viewMulti" pfd
   gns <- viewGamesTab (map G._game $ _games $ _multi s) isAdmin dataDir
   mgn <- liftRouteT $ lift $ getPlayersGame pn s
   vg <- case mgn of
      Just g -> viewGame (G._game g) playAs (_pLastRule $ fromJustNote "viewMulti" pfd) isAdmin
      Nothing -> ok $ h3 "Not viewing any game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ vg

viewGamesTab :: [Game] -> Bool -> FilePath -> RoutedNomyxServer Html
viewGamesTab gs isAdmin dataDir = do
   gns <- mapM (viewGameName isAdmin) gs
   newGameLink <- showURL NewGame
   settingsLink <- showURL W.PlayerSettings
   advLink <- showURL Advanced
   logoutURL  <- showURL (U_AuthProfile $ AuthURL A_Logout)
   mods <- liftIO $ getDirectoryContents $ dataDir </> modDir
   fmods <- liftIO $ filterM (getFileStatus . (\f -> joinPath [dataDir, modDir, f]) >=> return . isRegularFile) $ mods
   ok $ do
      h3 "Main menu" >> br
      "Active games:" >> br
      table $ do
         case gs of
            [] -> tr $ td "No Games"
            _ ->  sequence_ gns
      when isAdmin $ H.a "Create a new game" ! (href $ toValue newGameLink) >> br
      br >> "Nomyx language files:" >> br
      H.a "Rules examples"    ! (href $ "/src/Language/Nomyx/Examples.hs") >> br
      H.a "Basic rules"       ! (href $ "/src/Language/Nomyx/Rule.hs") >> br
      H.a "Rules definitions" ! (href $ "/src/Language/Nomyx/Definition.hs") >> br
      H.a "Rules types"       ! (href $ "/src/Language/Nomyx/Expression.hs") >> br
      H.a "Voting system"     ! (href $ "/src/Language/Nomyx/Vote.hs") >> br
      when (fmods /= []) $ do
         br >> "Uploaded files:" >> br
         mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : modDir </> f)) >> br) (sort fmods)
      br >> "Settings:" >> br
      H.a "Player settings" ! (href $ toValue settingsLink) >> br
      H.a "Advanced"        ! (href $ toValue advLink) >> br
      H.a "Logout"          ! (href $ toValue logoutURL) >> br


viewGameName :: Bool -> Game -> RoutedNomyxServer Html
viewGameName isAdmin g = do
   let gn = _gameName g
   join  <- showURL (JoinGame gn)
   leave <- showURL (LeaveGame gn)
   view  <- showURL (ViewGame gn)
   del  <- showURL (DelGame gn)
   ok $ tr $ do
      td ! A.id "gameName" $ string $ (gn ++ "   ")
      td $ H.a "View"  ! (href $ toValue view) ! (A.title $ toValue Help.view)
      td $ H.a "Join"  ! (href $ toValue $ "#openModalJoin" ++ gn) ! (A.title $ toValue Help.join)
      td $ H.a "Leave" ! (href $ toValue $ "#openModalLeave" ++ gn)
      when isAdmin $ td $ H.a "Del"   ! (href $ toValue del)
      div ! A.id (toValue $ "openModalJoin" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 "Joining the game. Please register in the Agora (see the link) and introduce yourself to the other players! \n \
                If you do not wich to play, you can just view the game."
            H.a "Join" ! (href $ toValue join) ! A.class_ "modalButton" ! (A.title $ toValue Help.join)
            H.a "View" ! (href $ toValue view) ! A.class_ "modalButton" ! (A.title $ toValue Help.view)
      div ! A.id (toValue $ "openModalLeave" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 "Do you really want to leave? You will loose your assets in the game (for example, your bank account)."
            H.a "Leave" ! (href $ toValue leave) ! A.class_ "modalButton"
            H.a "Stay"  ! (href $ toValue view) ! A.class_ "modalButton"


nomyxPage :: (TVar Session) -> RoutedNomyxServer Response
nomyxPage ts = do
   pn <- getPlayerNumber ts
   s <- liftIO $ atomically $ readTVar ts
   let dataDir = _dataDir $ _mSettings $ _multi s
   name <- liftIO $ Utils.getPlayerName pn s
   playAs <- getPlayAs ts
   m <- viewMulti pn playAs dataDir s
   let body = do
       string $ "Welcome to Nomyx, " ++ name ++ "! "
       when (playAs /= pn) $ (b ! A.style "color:red;" $ string ("Playing as Player #" ++ (show playAs)) )
   mainPage' "Welcome to Nomyx!"
            body
            (H.div ! A.id "multi" $ m)
            False

nomyxSite :: (TVar Session) -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite tm = setDefault HomePage $ mkSitePI (runRouteT $ routedNomyxCommands tm)

routedNomyxCommands :: (TVar Session) -> PlayerCommand -> RoutedNomyxServer Response
routedNomyxCommands ts (U_AuthProfile auth)  = authenticate      ts auth
routedNomyxCommands ts PostAuth              = postAuthenticate  ts
routedNomyxCommands ts HomePage              = homePage          ts
routedNomyxCommands ts MainPage              = nomyxPage         ts
routedNomyxCommands ts (JoinGame game)       = joinGame          ts game
routedNomyxCommands ts (LeaveGame game)      = leaveGame         ts game
routedNomyxCommands ts (ViewGame game)       = viewGamePlayer    ts game
routedNomyxCommands ts (DelGame game)        = delGame           ts game
routedNomyxCommands ts (NewRule game)        = newRule           ts game
routedNomyxCommands _  NewGame               = newGamePage
routedNomyxCommands ts SubmitNewGame         = newGamePost       ts
routedNomyxCommands ts (DoInput en game)     = newInput          ts en game
routedNomyxCommands ts Upload                = newUpload         ts
routedNomyxCommands ts W.PlayerSettings      = playerSettings    ts
routedNomyxCommands ts SubmitPlayerSettings  = newPlayerSettings ts
routedNomyxCommands ts Advanced              = advanced          ts
routedNomyxCommands ts SubmitPlayAs          = newPlayAsSettings ts
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
  let d = _dataDir $ _mSettings $ _multi s
  let d' = _sourceDir $ _mSettings $ _multi s
  mconcat [
    serveDirectory DisableBrowsing [] d,
    serveDirectory DisableBrowsing [] d',
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack (nomyxURL net)) "/Nomyx" (nomyxSite ts)
       return $ toResponse html]


