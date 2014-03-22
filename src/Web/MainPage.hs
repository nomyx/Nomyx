{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Happstack.Auth
import Safe
import Paths_Nomyx_Language
import Profile
import Control.Monad.Error
import Control.Applicative
default (Integer, Double, Data.Text.Text)

viewMulti :: PlayerNumber -> FilePath -> Session -> RoutedNomyxServer Html
viewMulti pn saveDir s = do
   pfd <- getProfile s pn
   let isAdmin = _pIsAdmin $ fromJustNote "viewMulti" pfd
   gns <- viewGamesTab (_gameInfos $ _multi s) isAdmin saveDir pn
   mgi <- liftRouteT $ lift $ getPlayersGame pn s
   vg <- case mgi of
      Just gi -> viewGameInfo gi pn (_pLastRule $ fromJustNote "viewMulti" pfd) isAdmin
      Nothing -> ok $ h3 "Not viewing any game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ vg

viewGamesTab :: [GameInfo] -> Bool -> FilePath -> PlayerNumber -> RoutedNomyxServer Html
viewGamesTab gis isAdmin saveDir pn = do
   let canCreateGame = isAdmin || numberOfGamesOwned gis pn < 1
   let publicPrivate = partition ((== True) . _isPublic) gis
   let vgi gi = viewGameName isAdmin canCreateGame pn gi
   public <- mapM vgi (fst publicPrivate)
   private <- mapM vgi (snd publicPrivate)
   newGameLink  <- showURL NewGame
   settingsLink <- showURL W.PlayerSettings
   advLink      <- showURL Advanced
   logoutURL    <- showURL (U_AuthProfile $ AuthURL A_Logout)
   fmods <- liftIO $ getUploadedModules saveDir
   ok $ do
      h3 "Main menu" >> br
      case public of
         [] -> b "No public games"
         p -> do
            b "Public games:"
            table $ sequence_ p
      br
      case private of
         [] -> ""
         p -> do
            b "Private games:"
            table $ sequence_ p
      br
      when canCreateGame $ H.a "Create a new game" ! (href $ toValue newGameLink) >> br
      br >> "Help files:" >> br
      H.a "Rules examples"    ! (href $ "/html/Language-Nomyx-Examples.html") ! (target "_blank") >> br
      H.a "Nomyx language"    ! (href $ "/html/Language-Nomyx.html") ! (target "_blank") >> br
      when (fmods /= []) $ do
         br >> "Uploaded files:" >> br
         mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : uploadDir </> f)) >> br) (sort fmods)
      br >> "Settings:" >> br
      H.a "Player settings" ! (href $ toValue settingsLink) >> br
      H.a "Advanced"        ! (href $ toValue advLink) >> br
      H.a "Logout"          ! (href $ toValue logoutURL) >> br


viewGameName :: Bool -> Bool -> PlayerNumber -> GameInfo -> RoutedNomyxServer Html
viewGameName isAdmin canCreateGame pn gi = do
   let g = getGame gi
   let isGameAdmin = isAdmin || maybe False (==pn) (_ownedBy gi)
   let gn = _gameName g
   let canFork = canCreateGame
   let canDel = isGameAdmin
   let canView = isGameAdmin || _isPublic gi
   main  <- showURL (W.MainPage)
   join  <- showURL (W.JoinGame gn)
   leave <- showURL (W.LeaveGame gn)
   view  <- showURL (W.ViewGame gn)
   del   <- showURL (W.DelGame gn)
   fork  <- showURL (W.ForkGame gn)
   if canView then
    ok $ tr $ do
      let cancel = H.a "Cancel" ! (href $ toValue main) ! A.class_ "modalButton"
      td ! A.id "gameName" $ string $ (gn ++ "   ")
      td $ H.a "View"  ! (href $ toValue view) ! (A.title $ toValue Help.view)
      td $ H.a "Join"  ! (href $ toValue $ "#openModalJoin" ++ gn) ! (A.title $ toValue Help.join)
      td $ H.a "Leave" ! (href $ toValue $ "#openModalLeave" ++ gn)
      when canDel $ td $ H.a "Del"   ! (href $ toValue del)
      when canFork $ td $ H.a "Fork"  ! (href $ toValue $ "#openModalFork" ++ gn)
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
   name <- liftIO $ Profile.getPlayerName pn s
   pn <- getPlayerNumber ts
   m <- viewMulti pn saveDir s
   mainPage' "Welcome to Nomyx!"
            (string $ "Welcome to Nomyx, " ++ name ++ "! ")
            (H.div ! A.id "multi" $ m)
            False

nomyxSite :: (TVar Session) -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite tm = setDefault HomePage $ mkSitePI (runRouteT $ catchRouteError . flip routedNomyxCommands tm)

routedNomyxCommands ::  PlayerCommand -> (TVar Session) ->RoutedNomyxServer Response
routedNomyxCommands (U_AuthProfile auth)  = authenticate      auth
routedNomyxCommands PostAuth              = postAuthenticate
routedNomyxCommands HomePage              = homePage
routedNomyxCommands MainPage              = nomyxPage
routedNomyxCommands (W.JoinGame game)     = joinGame          game
routedNomyxCommands (W.LeaveGame game)    = leaveGame         game
routedNomyxCommands (ViewGame game)       = viewGamePlayer    game
routedNomyxCommands (DelGame game)        = delGame           game
routedNomyxCommands (ForkGame game)       = forkGame          game
routedNomyxCommands (NewRule game)        = newRule           game
routedNomyxCommands NewGame               = newGamePage
routedNomyxCommands SubmitNewGame         = newGamePost
routedNomyxCommands (DoInput en game)     = newInput          en game
routedNomyxCommands Upload                = newUpload
routedNomyxCommands W.PlayerSettings      = playerSettings
routedNomyxCommands SubmitPlayerSettings  = newPlayerSettings
routedNomyxCommands Advanced              = advanced
routedNomyxCommands (SubmitPlayAs game)   = newPlayAs         game
routedNomyxCommands SubmitAdminPass       = newAdminPass
routedNomyxCommands SubmitSettings        = newSettings
routedNomyxCommands SaveFilePage          = saveFilePage

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

catchRouteError :: RoutedNomyxServer Response -> RoutedNomyxServer Response
catchRouteError page = page `catchError` (const backToLogin) where

backToLogin :: RoutedNomyxServer Response
backToLogin = toResponse <$> do
   link <- showURL HomePage
   seeOther link $ string "Redirecting..."

getDocDir :: IO FilePath
getDocDir = do
   datadir <- getDataDir
   let (as, _:bs) = break (== "share") $ splitDirectories datadir
   return $ joinPath $ as ++ ["share", "doc"] ++ bs

