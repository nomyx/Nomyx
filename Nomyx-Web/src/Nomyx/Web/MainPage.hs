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
import Happstack.Authenticate.Core
import Happstack.Authenticate.Route (initAuthentication)
import Happstack.Authenticate.OpenId.Route (initOpenId)
import Happstack.Authenticate.Password.Route (initPassword)
import Happstack.Authenticate.Password.Controllers(usernamePasswordCtrl)
import Happstack.Authenticate.OpenId.Controllers(openIdCtrl)
import Paths_Nomyx_Language
import Control.Monad.Error
import Control.Applicative
import Safe
import Data.Acid (AcidState, query)

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
   logoutURL    <- defLink (Auth $ Controllers) (isJust mpn)
   loginURL     <- showURL Login
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
   vgd <- viewGameDesc g  mpn playAs isGameAdmin
   ok $ div ! A.id     (fromString $ (getElementName VisGame gn) ++ "Div")
            ! A.class_ (fromString $ ((getGroupName VisGame) ++ "Div") ++ " game") $ do
      div ! A.id "titleBar" $ do
         let attr :: String -> Attribute
             attr name = A.id     (fromString $ (getElementName (VisGameTabs gn) name) ++ "Button")
                      <> A.class_ (fromString $ (getGroupName (VisGameTabs gn)) ++ "Button" ++ " button")
                      <> onclick  (fromString $ setDivVisibilityAndSave (getGroupName (VisGameTabs gn)) (getElementName (VisGameTabs gn) name))
         H.a "Description "    ! attr "gameDesc" ! A.style "fontWeight:bold;"
         H.a "Rules "          ! attr "rules"
         H.a "Inputs/Outputs " ! attr "ios"
         H.a "New rule "       ! attr "newRule"
         H.a "Details "        ! attr "details"
      let attr name = A.id     (fromString $ (getElementName (VisGameTabs gn) name) ++ "Div")
                   <> A.class_ (fromString $ (getGroupName (VisGameTabs gn)) ++ "Div" ++ " gameBox")
      div ! attr "gameDesc" ! A.style "display:inline;" $ vgd
      div ! attr "rules"    ! A.style "display:none;"   $ viewAllRules g
      div ! attr "ios"      ! A.style "display:none;"   $ vios
      div ! attr "newRule"  ! A.style "display:none;"   $ rf
      div ! attr "details"  ! A.style "display:none;"   $ viewDetails pn g

data VisLevel = VisGame
              | VisGameTabs GameName
              deriving (Show)

getElementName :: VisLevel -> String -> String
getElementName vs name = (getGroupName vs) ++ "-" ++ (filter (/=' ') name)

getGroupName :: VisLevel -> String
getGroupName VisGame = "Game"
getGroupName (VisGameTabs gn) = "GameTabs" ++ (filter (/=' ') gn)

viewGameName :: Bool -> (Maybe PlayerNumber) -> GameInfo -> RoutedNomyxServer Html
viewGameName isAdmin mpn gi = do
   let g = getGame gi
   let isGameAdmin = isAdmin || maybe False (==mpn) (Just $ _ownedBy gi)
   let gn = _gameName g
   let canView = isGameAdmin || _isPublic gi
   ok $ when canView $ do
      let attr = A.id     (fromString $ ((getElementName VisGame gn) ++ "Button"))
              <> A.class_ (fromString $ ((getGroupName VisGame) ++ "Button") ++ " button")
              <> onclick  (fromString $ setDivVisibilityAndSave (getGroupName VisGame) (getElementName VisGame gn))
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
            False
            (H.div ! A.id "multi" $ m)


nomyxSite :: TVar Session
          -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
          -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite tm routeAuthenticate = setDefault MainPage $ mkSitePI $ runRouteT $ (\r -> catchRouteError $ routedNomyxCommands r routeAuthenticate tm)

routedNomyxCommands ::  PlayerCommand
                     -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
                     -> (TVar Session)
                     -> RoutedNomyxServer Response
--routedNomyxCommands NotLogged            _ = notLogged

routedNomyxCommands (Auth auth)        routeAuthenticate = authenticate auth routeAuthenticate
routedNomyxCommands Login                _ = Nomyx.Web.Login.login
routedNomyxCommands ResetPassword        _ = resetPasswordPage
routedNomyxCommands ChangePassword       _ = changePasswordPanel
routedNomyxCommands OpenIdRealm          _ = openIdRealmPanel
--routedNomyxCommands PostAuth             _ = postAuthenticate
routedNomyxCommands MainPage             _ = nomyxPage
routedNomyxCommands (W.JoinGame game)    _ = joinGame          game
routedNomyxCommands (W.LeaveGame game)   _ = leaveGame         game
routedNomyxCommands (DelGame game)       _ = delGame           game
routedNomyxCommands (NewRule game)       _ = newRule           game
routedNomyxCommands NewGame              _ = newGamePage
routedNomyxCommands SubmitNewGame        _ = newGamePost
routedNomyxCommands (DoInput en fa ft g) _ = newInput en fa ft g
routedNomyxCommands Upload               _ = newUpload
routedNomyxCommands W.PlayerSettings     _ = playerSettings
routedNomyxCommands SubmitPlayerSettings _ = newPlayerSettings
routedNomyxCommands Advanced             _ = advanced
routedNomyxCommands (SubmitPlayAs game)  _ = newPlayAs         game
routedNomyxCommands SubmitAdminPass      _ = newAdminPass
routedNomyxCommands SubmitSettings       _ = newSettings
routedNomyxCommands SaveFilePage         _ = saveFilePage
routedNomyxCommands NomyxJS              _ = ok . toResponse . nomyxJS

launchWebServer :: TVar Session -> Network -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> IO ()
launchWebServer tm net routeAuthenticate = do
   putStrLn $ "Starting web server...\nTo connect, drive your browser to \"" ++ nomyxURL net ++ "/Nomyx\""
   simpleHTTP nullConf {HS.port = T._port net} $ server tm net routeAuthenticate

--serving Nomyx web page as well as data from this package and the language library package
server :: TVar Session -> Network -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> ServerPartT IO Response
server ts net routeAuthenticate = do
  s <- liftIO $ atomically $ readTVar ts
  let set = _mSettings $ _multi s
  docdir <- liftIO getDocDir


  -- liftIO $ atomically $ writeTVar ts (s {_profiles = (Profiles authenticateState (acidProfileData $ _profiles s) )})
  as <- liftIO $ query (acidAuth $ _profiles s) GetAuthenticateState
  liftIO $ print as
  mconcat [
    serveDirectory EnableBrowsing [] (_saveDir set),
    serveDirectory EnableBrowsing [] docdir,
    serveDirectory EnableBrowsing [] (_webDir set),
    serveDirectory EnableBrowsing [] (_sourceDir set),
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack (nomyxURL net)) "/Nomyx" (nomyxSite ts routeAuthenticate)
       return $ toResponse html]

catchRouteError :: RoutedNomyxServer Response -> RoutedNomyxServer Response
catchRouteError page = page `catchError` const backToLogin where

backToLogin :: RoutedNomyxServer Response
backToLogin = toResponse <$> do
   link <- showURL Login
   seeOther link ("Redirecting..." :: String)

getDocDir :: IO FilePath
getDocDir = do
   datadir <- getDataDir
   let (as, _:bs) = break (== "share") $ splitDirectories datadir
   return $ joinPath $ as ++ ["share", "doc"] ++ bs
