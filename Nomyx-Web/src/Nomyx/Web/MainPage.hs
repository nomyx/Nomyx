{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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
import Nomyx.Web.Common as W
import Nomyx.Web.Settings
import Nomyx.Web.NewGame
import Nomyx.Web.Login
import Nomyx.Web.Game.Infos
import Nomyx.Web.Game.Rules
import Nomyx.Web.Game.IOs
import Nomyx.Web.Game.NewRule
import Nomyx.Web.Game.Details
import Nomyx.Core.Profile as Profile
import Nomyx.Core.Utils
import Nomyx.Core.Types as T
import Nomyx.Core.Engine
import qualified Nomyx.Core.Session as S
import Data.List
import Data.Text(Text, pack)
import Happstack.Auth
import Paths_Nomyx_Language
import Control.Monad.Error
import Control.Applicative
import Safe

default (Integer, Double, Data.Text.Text)

viewMulti :: (Maybe PlayerNumber) -> FilePath -> Session -> RoutedNomyxServer Html
viewMulti mpn saveDir s = do
   (isAdmin, lr) <- case mpn of
      Just pn -> do
         pfd <- getProfile s pn
         let isAdmin = _pIsAdmin $ fromJustNote "viewMulti" pfd
         let lr = _pLastRule $ fromJustNote "viewMulti" pfd
         return (isAdmin, lr)
      Nothing -> return (False, Nothing)
   let gis = _gameInfos $ _multi s
   gns <- viewGamesTab (_gameInfos $ _multi s) isAdmin saveDir mpn
   vgs <- mapM (\gi -> viewGameInfo gi mpn lr isAdmin) gis
   ok $ do
      div ! A.id "gameList" $ gns
      sequence_ vgs

viewGamesTab :: [GameInfo] -> Bool -> FilePath -> (Maybe PlayerNumber) -> RoutedNomyxServer Html
viewGamesTab gis isAdmin saveDir mpn = do
   let canCreateGame = maybe False (\pn -> isAdmin || numberOfGamesOwned gis pn < 1) mpn
   let publicPrivate = partition ((== True) . _isPublic) gis
   let vgi = viewGameName isAdmin mpn
   public <- mapM vgi (fst publicPrivate)
   private <- mapM vgi (snd publicPrivate)
   newGameLink  <- defLink NewGame (isJust mpn)
   settingsLink <- defLink W.PlayerSettings (isJust mpn)
   advLink      <- defLink Advanced (isJust mpn)
   logoutURL    <- defLink (Auth $ AuthURL A_Logout) (isJust mpn)
   loginURL     <- showURL (Auth $ AuthURL A_Login)
   fmods <- liftIO $ getUploadedModules saveDir
   ok $ do
      h3 "Main menu" >> br
      case public of
         [] -> b "No public games"
         p:ps -> do
            b "Public games:"
            table $ do
               p ! A.style "font-weight:bold;"
               sequence_ ps
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

viewGameInfo :: GameInfo -> (Maybe PlayerNumber) -> Maybe LastRule -> Bool -> RoutedNomyxServer Html
viewGameInfo gi mpn mlr isAdmin = do
   let g = getGame gi
   let gn = _gameName g
   (pi, isGameAdmin, playAs, pn) <- case mpn of
      Just pn -> do
         let pi = Profile.getPlayerInfo g pn
         let isGameAdmin = isAdmin || maybe False (== pn) (_ownedBy gi)
         let playAs = maybe Nothing _playAs pi
         return (pi, isGameAdmin, playAs, pn)
      Nothing -> return (Nothing, False, Nothing, 0)
   rf <- viewRuleForm mlr (isJust pi) isGameAdmin (_gameName g)
   vios <- viewIOs (fromMaybe pn playAs) g
   vgd <- viewGameDesc g  (isJust mpn) playAs isGameAdmin
   ok $ div ! A.id (fromString $ getIdBox "" gn) ! A.class_ (fromString $ (getClassBox "" "game") ++ " game") $ do
      div ! A.id "titleBar" $ do
         let attr :: String -> Attribute
             attr name = A.id (fromString $ getIdButton gn name) <> A.class_ (fromString $ (getClassButton gn "gameBox") ++ " button") <> onclick (fromString $ divVisibility gn name "gameBox")
         H.a "Description "    ! attr "gameDesc" ! A.style "fontWeight:bold;"
         H.a "Rules "          ! attr "rules"
         H.a "Inputs/Outputs " ! attr "ios"
         H.a "New rule "       ! attr "newRule"
         H.a "Details "        ! attr "details"
      let attr name = A.id (fromString $ getIdBox gn name) <> A.class_ (fromString $ (getClassBox gn "gameBox") ++ " gameBox")
      div ! attr "gameDesc" ! A.style "display:inline;" $ vgd
      div ! attr "rules"    ! A.style "display:none;"   $ viewAllRules g
      div ! attr "ios"      ! A.style "display:none;"   $ vios
      div ! attr "newRule"  ! A.style "display:none;"   $ rf
      div ! attr "details"  ! A.style "display:none;"   $ viewDetails pn g

viewGameName :: Bool -> (Maybe PlayerNumber) -> GameInfo -> RoutedNomyxServer Html
viewGameName isAdmin mpn gi = do
   let g = getGame gi
   let isGameAdmin = isAdmin || maybe False (==mpn) (Just $ _ownedBy gi)
   let gn = _gameName g
   let canView = isGameAdmin || _isPublic gi
   ok $ when canView $ do
      let attr = A.id (fromString $ getIdButton "" gn) <> A.class_ (fromString $ (getClassButton "" "game") ++ " button") <> onclick (fromString $ divVisibility "" gn "game")
      tr $ td $ H.a (fromString (gn ++ "   ")) ! (A.title $ toValue Help.view) ! attr


joinGame :: GameName -> TVar Session -> RoutedNomyxServer Response
joinGame gn ts = do
   pn <- fromJust <$> getPlayerNumber ts
   webCommand ts (S.joinGame gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

leaveGame :: GameName -> TVar Session -> RoutedNomyxServer Response
leaveGame gn ts = do
   pn <- fromJust <$> getPlayerNumber ts
   webCommand ts (S.leaveGame gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

delGame :: GameName -> TVar Session -> RoutedNomyxServer Response
delGame gn ts = do
   webCommand ts (S.delGame gn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

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
routedNomyxCommands (DelGame game)        = delGame           game
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

