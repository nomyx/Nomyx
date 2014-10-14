{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nomyx.Web.MainPage (launchWebServer) where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.Site
import Web.Routes.PathInfo
import Web.Routes.Happstack
import Web.Routes.RouteT
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Data.Maybe
import Data.String
import Control.Concurrent.STM
import Language.Nomyx
import Happstack.Server as HS
import System.FilePath
import qualified Nomyx.Web.Help as Help
import Nomyx.Web.Game
import Nomyx.Web.Common as W
import Nomyx.Web.Settings
import Nomyx.Web.NewGame
import Nomyx.Web.Login
import Data.List
import Data.Text(Text, pack)
import Happstack.Auth
import Paths_Nomyx_Language
import Control.Monad.Error
import Control.Applicative
import Safe
import Nomyx.Core.Profile as Profile
import Nomyx.Core.Utils
import Nomyx.Core.Types as T
import Nomyx.Core.Engine

default (Integer, Double, Data.Text.Text)

viewMulti :: (Maybe PlayerNumber) -> FilePath -> Session -> RoutedNomyxServer Html
viewMulti mpn saveDir s = do
   (isAdmin, mgi, lr) <- case mpn of
      Just pn -> do
         pfd <- getProfile s pn
         let isAdmin = _pIsAdmin $ fromJustNote "viewMulti" pfd
         mgi <- liftRouteT $ lift $ getPlayersGame pn s
         let lr = _pLastRule $ fromJustNote "viewMulti" pfd
         return (isAdmin, mgi, lr)
      Nothing -> return (False, getFirstGame s, Nothing)
   gns <- viewGamesTab (_gameInfos $ _multi s) isAdmin saveDir mpn
   vg <- case mgi of
            Just gi -> viewGameInfo gi mpn lr isAdmin
            Nothing -> ok $ h3 "Not viewing any game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ vg

viewGamesTab :: [GameInfo] -> Bool -> FilePath -> (Maybe PlayerNumber) -> RoutedNomyxServer Html
viewGamesTab gis isAdmin saveDir mpn = do
   let canCreateGame = maybe False (\pn -> isAdmin || numberOfGamesOwned gis pn < 1) mpn
   let publicPrivate = partition ((== True) . _isPublic) gis
   let vgi = viewGameName isAdmin canCreateGame mpn
   let defLink a = if (isJust mpn) then showURL a else showURL (Auth $ AuthURL A_Login)
   public <- mapM vgi (fst publicPrivate)
   private <- mapM vgi (snd publicPrivate)
   newGameLink  <- defLink NewGame
   settingsLink <- defLink W.PlayerSettings
   advLink      <- defLink Advanced
   logoutURL    <- defLink (Auth $ AuthURL A_Logout)
   loginURL     <- showURL (Auth $ AuthURL A_Login)
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
      br >> b "Help files:" >> br
      H.a "Rules examples"    ! (href "/html/Language-Nomyx-Examples.html") ! target "_blank" >> br
      H.a "Nomyx language"    ! (href "/html/Language-Nomyx.html") ! target "_blank" >> br
      when (fmods /= []) $ do
         br >> b "Uploaded files:" >> br
         mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : uploadDir </> f)) >> br) (sort fmods)
      br >> b "Settings:" >> br
      H.a "Player settings" ! (href $ toValue settingsLink) >> br
      H.a "Advanced"        ! (href $ toValue advLink) >> br
      H.a "Logout"          ! (href $ toValue logoutURL) >> br
      H.a "Login"           ! (href $ toValue loginURL) >> br


viewGameName :: Bool -> Bool -> (Maybe PlayerNumber) -> GameInfo -> RoutedNomyxServer Html
viewGameName isAdmin canCreateGame mpn gi = do
   let g = getGame gi
   let isGameAdmin = isAdmin || maybe False (==mpn) (Just $ _ownedBy gi)
   let gn = _gameName g
   let canFork = canCreateGame
   let canDel = isGameAdmin
   let canView = isGameAdmin || _isPublic gi
   let link a = if (isJust mpn) then showURL a else showURL (Auth $ AuthURL A_Login)
   main  <- showURL W.MainPage
   join  <- link (W.JoinGame gn)
   leave <- link (W.LeaveGame gn)
   view  <- link (W.ViewGame gn)
   del   <- link (W.DelGame gn)
   fork  <- link (W.ForkGame gn)
   ok $ if canView then tr $ do
      let cancel = H.a "Cancel" ! (href $ toValue main) ! A.class_ "modalButton"
      td ! A.id "gameName" $ fromString (gn ++ "   ")
      td $ H.a "View"  ! (href $ toValue view) ! (A.title $ toValue Help.view)
      td $ H.a "Join"  ! (href $ toValue $ "#openModalJoin" ++ gn) ! (A.title $ toValue Help.join)
      td $ H.a "Leave" ! (href $ toValue $ "#openModalLeave" ++ gn)
      when canDel $ td $ H.a "Del"   ! (href $ toValue del)
      when canFork $ td $ H.a "Fork"  ! (href $ toValue $ "#openModalFork" ++ gn)
      div ! A.id (toValue $ "openModalJoin" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 $ fromString $ "Joining the game. Please register in the Agora (see the link) and introduce yourself to the other players! \n" ++
               "If you do not whish to play, you can just view the game."
            cancel
            H.a "Join" ! (href $ toValue join) ! A.class_ "modalButton" ! (A.title $ toValue Help.join)
      div ! A.id (toValue $ "openModalLeave" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 "Do you really want to leave? You will loose your assets in the game (for example, your bank account)."
            cancel
            H.a "Leave" ! (href $ toValue leave) ! A.class_ "modalButton"
      div ! A.id (toValue $ "openModalFork" ++ gn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 $ fromString $ "Fork game \"" ++ gn ++ "\"? This will create a new game based on the previous one. You will be able to test \n" ++
               "your new rules independently of the original game. The new game is completely private: you will be alone. Please delete it when finished."
            cancel
            H.a "Fork" ! (href $ toValue fork) ! A.class_ "modalButton"
   else ""

nomyxPage :: TVar Session -> RoutedNomyxServer Response
nomyxPage ts = do
   mpn <- getPlayerNumber ts
   s <- liftIO $ atomically $ readTVar ts
   let saveDir = _saveDir $ _mSettings $ _multi s
   name <- case mpn of
      Just pn -> liftIO $ Profile.getPlayerName pn s
      Nothing -> return "Guest"
   m <- viewMulti mpn saveDir s
   mainPage' "Welcome to Nomyx!"
            (fromString $ "Welcome to Nomyx, " ++ name ++ "! ")
            (H.div ! A.id "multi" $ m)
            False

nomyxSite :: TVar Session -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite tm = setDefault MainPage $ mkSitePI (runRouteT $ catchRouteError . flip routedNomyxCommands tm)

routedNomyxCommands ::  PlayerCommand -> (TVar Session) -> RoutedNomyxServer Response
routedNomyxCommands NotLogged             = notLogged
routedNomyxCommands (Auth auth)           = authenticate      auth
routedNomyxCommands PostAuth              = postAuthenticate
routedNomyxCommands MainPage              = nomyxPage
routedNomyxCommands (W.JoinGame game)     = joinGame          game
routedNomyxCommands (W.LeaveGame game)    = leaveGame         game
routedNomyxCommands (ViewGame game)       = viewGamePlayer    game
routedNomyxCommands (DelGame game)        = delGame           game
routedNomyxCommands (ForkGame game)       = forkGame          game
routedNomyxCommands (NewRule game)        = newRule           game
routedNomyxCommands NewGame               = newGamePage
routedNomyxCommands SubmitNewGame         = newGamePost
routedNomyxCommands (DoInput en fa ft g)  = newInput en fa ft g
routedNomyxCommands Upload                = newUpload
routedNomyxCommands W.PlayerSettings      = playerSettings
routedNomyxCommands SubmitPlayerSettings  = newPlayerSettings
routedNomyxCommands Advanced              = advanced
routedNomyxCommands (SubmitPlayAs game)   = newPlayAs         game
routedNomyxCommands SubmitAdminPass       = newAdminPass
routedNomyxCommands SubmitSettings        = newSettings
routedNomyxCommands SaveFilePage          = saveFilePage

launchWebServer :: TVar Session -> Network -> IO ()
launchWebServer tm net = do
   putStrLn $ "Starting web server...\nTo connect, drive your browser to \"" ++ nomyxURL net ++ "/Nomyx\""
   simpleHTTP nullConf {HS.port = T._port net} $ server tm net

--serving Nomyx web page as well as data from this package and the language library package
server :: TVar Session -> Network -> ServerPartT IO Response
server ts net = do
  s <- liftIO $ atomically $ readTVar ts
  let set = _mSettings $ _multi s
  docdir <- liftIO getDocDir
  mconcat [
    serveDirectory EnableBrowsing [] (_saveDir set),
    serveDirectory EnableBrowsing [] docdir,
    serveDirectory EnableBrowsing [] (_webDir set),
    serveDirectory EnableBrowsing [] (_sourceDir set),
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack (nomyxURL net)) "/Nomyx" (nomyxSite ts)
       return $ toResponse html]

catchRouteError :: RoutedNomyxServer Response -> RoutedNomyxServer Response
catchRouteError page = page `catchError` const backToLogin where

backToLogin :: RoutedNomyxServer Response
backToLogin = toResponse <$> do
   link <- showURL NotLogged
   seeOther link ("Redirecting..." :: String)

getDocDir :: IO FilePath
getDocDir = do
   datadir <- getDataDir
   let (as, _:bs) = break (== "share") $ splitDirectories datadir
   return $ joinPath $ as ++ ["share", "doc"] ++ bs

