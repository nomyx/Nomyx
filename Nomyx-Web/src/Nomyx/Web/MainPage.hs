{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Nomyx.Web.MainPage (launchWebServer) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                             as T (Text, pack, length)
import           Happstack.Authenticate.OpenId.Route   (initOpenId)
import           Happstack.Authenticate.Password.Route (initPassword)
import           Happstack.Authenticate.Password.Core(PasswordConfig(..), PasswordState)
import           Happstack.Authenticate.Route          (initAuthentication)
import           Happstack.Authenticate.Core (AuthenticateURL(..), AuthenticateConfig(..), AuthenticateState, Email(..), User(..), Username(..), UserId(..), GetAuthenticateState(..), decodeAndVerifyToken, tokenUser, usernamePolicy)
import           Happstack.Server                      as HS
import           Language.Nomyx
import           Nomyx.Core.Engine                     hiding (JoinGame,
                                                        LeaveGame)
import           Nomyx.Core.Profile                    as Profile
import qualified Nomyx.Core.Session                    as S
import           Nomyx.Core.Types                      as T hiding (Library)
import           Nomyx.Core.Utils
import           Nomyx.Web.Common                      as W
import           Nomyx.Web.Game.Details
import           Nomyx.Web.Game.Infos
import           Nomyx.Web.Game.Actions
import           Nomyx.Web.Game.Templates
import           Nomyx.Web.Game.Rules
import qualified Nomyx.Web.Help                        as Help
import           Nomyx.Web.Login
import           Nomyx.Web.NewGame
import           Nomyx.Web.Settings
import           Nomyx.Web.Types
import qualified Nomyx.Auth                            as Auth
import           Paths_Nomyx_Language
import           Prelude                               hiding (div)
import           Safe
import           System.FilePath
import           Text.Blaze.Html5                      hiding (head, map)
import qualified Text.Blaze.Html5                      as H
import           Text.Blaze.Html5.Attributes           hiding (dir, id)
import qualified Text.Blaze.Html5.Attributes           as A
import           Web.Routes.Happstack
import           Web.Routes.PathInfo
import           Web.Routes.RouteT                     (runRouteT)
import           Web.Routes.Site
import           Imprevu.Happstack.Forms
import           Imprevu.Happstack.Types


default (Integer, Double, T.Text)

viewMulti :: (Maybe PlayerNumber) -> FilePath -> GameTab -> GameName -> Session -> RoutedNomyxServer Html
viewMulti mpn saveDir gt gn s = do
   (isAdmin, lr) <- case mpn of
      Just pn -> do
         pfd <- getProfile s pn
         let isAdmin = _pIsAdmin $ fromJustNote "viewMulti" pfd
         let lr = _pLastRule $ fromJustNote "viewMulti" pfd
         return (isAdmin, lr)
      Nothing -> return (False, Nothing)
   let gi = fromJust $ getGameByName gn s  --TODO fix
   gns <- viewGamesTab gi isAdmin saveDir mpn
   vg <- viewGameInfo gi mpn lr isAdmin gt (_mTemplates $ _mLibrary $ _multi s)
   ok $ do
      div ! A.id "gameList" $ gns
      vg

viewGamesTab :: GameInfo -> Bool -> FilePath -> (Maybe PlayerNumber) -> RoutedNomyxServer Html
viewGamesTab gi isAdmin saveDir mpn = do
   let g = getGame gi
   let gn = _gameName g
   --fmods <- liftIO $ getUploadedModules saveDir
   ok $ do
     table $ do
       tr $ td ! A.class_ "buttonTD" $ H.a "Home "       ! A.class_ "button" ! href (toValue $ showRelURL $ Menu Home gn)
       tr $ td ! A.class_ "buttonTD" $ H.a "Rules "      ! A.class_ "button" ! href (toValue $ showRelURL $ Menu Rules gn)
       tr $ td ! A.class_ "buttonTD" $ H.a "My actions " ! A.class_ "button" ! href (toValue $ showRelURL $ Menu Actions gn)
       tr $ td ! A.class_ "buttonTD" $ H.a "Library "    ! A.class_ "button" ! href (toValue $ showRelURL $ Menu Library gn)
       tr $ td ! A.class_ "buttonTD" $ H.a "Details "    ! A.class_ "button" ! href (toValue $ showRelURL $ Menu Details gn)
     br >> b "Help files:" >> br
     H.a "Rules examples"    ! (href "/html/Language-Nomyx-Examples.html") ! target "_blank" >> br
     H.a "Nomyx language"    ! (href "/html/Language-Nomyx.html") ! target "_blank" >> br
     --when (fmods /= []) $ do
     --  br >> b "Uploaded files:" >> br
     --  mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : uploadDir </> f)) >> br) (sort fmods)
     br >> b "Settings:" >> br
     H.a "Advanced"        ! (href $ toValue $ defLink Advanced (isJust mpn)) >> br
     H.a "Logout"          ! (href $ toValue $ showRelURL Login) >> br
     H.a "Login"           ! (href $ toValue $ showRelURL Login) >> br

viewGameInfo :: GameInfo -> (Maybe PlayerNumber) -> Maybe LastRule -> Bool -> GameTab -> [RuleTemplate] -> RoutedNomyxServer Html
viewGameInfo gi mpn mlr isAdmin gt lib = do
   let g = getGame gi
   let gn = _gameName g
   let pi = join $ (Profile.getPlayerInfo g) <$> mpn
   let isGameAdmin = isAdmin || maybe False (== mpn) (Just $ _ownedBy gi)
   let playAs = mpn >> maybe Nothing _playAs pi
   let pn = fromMaybe 0 mpn
   vrf <- viewRuleTemplates lib mlr gn
   vios <- viewIOs (fromMaybe pn playAs) g
   vgd <- viewGameDesc g mpn playAs isGameAdmin
   vrs <- viewAllRules pn g
   ok $ case gt of
        Home    -> div ! A.id "gameDescGameDiv" ! A.class_ "game" $ vgd
        Rules   -> div ! A.id "rulesGameDiv"    ! A.class_ "game" $ vrs
        Actions -> div ! A.id "iosGameDiv"      ! A.class_ "game" $ vios
        Library -> div ! A.id "newRuleGameDiv"  ! A.class_ "game" $ vrf
        Details -> div ! A.id "detailsGameDiv"  ! A.class_ "game" $ viewDetails pn g

viewGames :: [GameInfo] -> Bool -> FilePath -> (Maybe PlayerNumber) -> Html
viewGames gis isAdmin saveDir mpn = do
   let canCreateGame = maybe False (\pn -> isAdmin || numberOfGamesOwned gis pn < 1) mpn
   let publicPrivate = partition ((== True) . _isPublic) gis
   let vgi = viewGameName isAdmin mpn
   let public = map vgi (fst publicPrivate)
   let private = map vgi (snd publicPrivate)
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
   when canCreateGame $ H.a "Create a new game" ! (href $ toValue $ defLink NewGame (isJust mpn)) >> br

viewGameName :: Bool -> (Maybe PlayerNumber) -> GameInfo -> Html
viewGameName isAdmin mpn gi = do
   let g = getGame gi
   let isGameAdmin = isAdmin || maybe False (==mpn) (Just $ _ownedBy gi)
   let gn = _gameName g
   let canView = isGameAdmin || _isPublic gi
   when canView $ tr $ td $ H.a (fromString (gn ++ "   ")) ! (A.title $ toValue Help.view) -- ! attr


joinGame :: GameName -> RoutedNomyxServer Response
joinGame gn = do
   pn <- fromJust <$> getPlayerNumber
   webCommand (S.joinGame gn pn)
   seeOther (showRelURL MainPage) $ toResponse "Redirecting..."

leaveGame :: GameName -> RoutedNomyxServer Response
leaveGame gn = do
   pn <- fromJust <$> getPlayerNumber
   webCommand (S.leaveGame gn pn)
   seeOther (showRelURL MainPage) $ toResponse "Redirecting..."

delGame :: GameName -> RoutedNomyxServer Response
delGame gn = do
   webCommand (S.delGame gn)
   seeOther (showRelURL MainPage) $ toResponse "Redirecting..."

nomyxPage :: Maybe GameName -> GameTab -> RoutedNomyxServer Response
nomyxPage mgn tab = do
   mpn <- getPlayerNumber
   s <- getSession
   let saveDir = _saveDir $ _mSettings $ _multi s
   name <- case mpn of
      Just pn -> liftIO $ Profile.getPlayerName pn s
      Nothing -> return "Guest"
   let gn = maybe (_gameName $ _game $ _loggedGame $ head $ _gameInfos $ _multi s) id mgn
   m <- viewMulti mpn saveDir tab gn s
   mainPage' "Welcome to Nomyx!"
            (fromString $ "Welcome to Nomyx, " ++ name ++ "! ")
            False
            (H.div ! A.id "multi" $ m)

nomyxSite :: WebSession -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite ws = setDefault MainPage $ mkSitePI $ (\a b -> evalStateT (runRouteT (catchRouteError . routedNomyxCommands) a b) ws)

routedNomyxCommands :: PlayerCommand -> RoutedNomyxServer Response
-- Authentication and login
routedNomyxCommands (Auth auth)          = authenticate auth
routedNomyxCommands Login                = loginPage
routedNomyxCommands Logout               = logout
routedNomyxCommands ResetPassword        = resetPasswordPage
routedNomyxCommands ChangePassword       = changePasswordPanel
routedNomyxCommands OpenIdRealm          = openIdRealmPanel
routedNomyxCommands PostAuth             = postAuthenticate
-- Game menu
routedNomyxCommands (Menu tab game)      = nomyxPage (Just game) tab
routedNomyxCommands MainPage             = nomyxPage Nothing Home
-- Game management
routedNomyxCommands (JoinGame game)      = joinGame          game
routedNomyxCommands (LeaveGame game)     = leaveGame         game
routedNomyxCommands (DelGame game)       = delGame           game
routedNomyxCommands NewGame              = newGamePage
routedNomyxCommands SubmitNewGame        = newGamePost
-- Game actions
routedNomyxCommands (DoInput en fa ft g) = newInput' en fa ft g
routedNomyxCommands (SubmitRule game)    = submitRuleTemplatePost game
-- Templates
routedNomyxCommands (NewRuleTemplate game) = newRuleTemplate game
routedNomyxCommands (DelRuleTemplate game rn) = delRuleTemplate game rn
-- File management
routedNomyxCommands Upload               = newUpload
--Settings
routedNomyxCommands Advanced             = advanced
routedNomyxCommands (SubmitPlayAs game)  = newPlayAs         game
routedNomyxCommands SubmitAdminPass      = newAdminPass
routedNomyxCommands SubmitSettings       = newSettings
routedNomyxCommands SaveFilePage         = saveFilePage
-- Misc
routedNomyxCommands NomyxJS              = ok $ toResponse nomyxJS

launchWebServer :: TVar Session -> Network -> IO ()
launchWebServer ts net = do
   putStrLn $ "Starting web server...\nTo connect, drive your browser to \"" ++ nomyxURL net ++ "/Nomyx\""
   s <- liftIO $ atomically $ readTVar ts
   let set = _mSettings $ _multi s
   let conf = nullConf {HS.port = T._port net}
   docdir <- liftIO getDocDir
   auth<- Auth.launchAuth (_saveDir set)
   let wst = WebState ts undefined undefined undefined
   simpleHTTP conf $ server (WebSession wst auth) set net docdir

--serving Nomyx web page as well as data from this package and the language library package
server :: WebSession -> Settings -> Network -> String -> ServerPartT IO Response
server ws set net docdir = mconcat [
    serveDirectory EnableBrowsing [] (_saveDir set),
    serveDirectory EnableBrowsing [] docdir,
    serveDirectory EnableBrowsing [] (_webDir set),
    serveDirectory EnableBrowsing [] (_sourceDir set),
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack $ nomyxURL net) "/Nomyx" (nomyxSite ws)
       return $ toResponse html]

catchRouteError :: RoutedNomyxServer Response -> RoutedNomyxServer Response
catchRouteError page = page `catchError` const backToLogin where

backToLogin :: RoutedNomyxServer Response
backToLogin = toResponse <$> seeOther (showRelURL Login) ("Redirecting..." :: String)

getDocDir :: IO FilePath
getDocDir = do
   datadir <- getDataDir
   let (as, _:bs) = break (== "share") $ splitDirectories datadir
   return $ joinPath $ as ++ ["share", "doc"] ++ bs
