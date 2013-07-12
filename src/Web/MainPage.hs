
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
import Paths_Nomyx as PN
import Paths_Nomyx_Language as PNL
import Control.Monad.State
import Data.Monoid
import Control.Concurrent.STM
import Language.Nomyx
import Text.Reform.Happstack
import Text.Reform
import Happstack.Server as HS
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Web.Help as Help
import Types as T
import Web.Game
import Web.Common
import Web.Settings
import Web.NewGame
import Web.Login
import qualified Text.Reform.Blaze.String as RB
import Utils
import Data.Maybe
import Data.Text(Text, pack)
import qualified Language.Nomyx.Game as G
import qualified Multi as M
import Happstack.Auth

default (Integer, Double, Data.Text.Text)


viewMulti :: PlayerNumber -> Session -> RoutedNomyxServer Html
viewMulti pn s = do
   pfd <- getProfile s pn
   gns <- viewGamesTab (map G._game $ _games $ _multi s) (pn == 1)
   mgn <- liftRouteT $ lift $ getPlayersGame pn s
   g <- case mgn of
      Just g -> viewGame (G._game g) pn (_pLastRule $ fromJust pfd)
      Nothing -> ok $ h3 "Not viewing any game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ g

viewGamesTab :: [Game] -> Bool -> RoutedNomyxServer Html
viewGamesTab gs admin = do
   gns <- mapM viewGameName gs
   newGameLink <- showURL NewGame
   uploadLink <- showURL Upload
   settingsLink <- showURL PSettings
   advLink <- showURL Advanced
   logoutURL  <- showURL (U_AuthProfile $ AuthURL A_Logout)
   up  <- lift $ viewForm "user" uploadForm
   dd <- lift $ lift $ PN.getDataDir
   mods <- lift $ lift $ getDirectoryContents $ dd </> modDir
   fmods <- lift $ lift $ filterM (getFileStatus . (\f -> joinPath [dd, modDir, f]) >=> return . isRegularFile) $ mods
   ok $ do
      h3 "Main menu" >> br
      "Active games:" >> br
      table $ do
         case gs of
            [] -> tr $ td "No Games"
            _ ->  sequence_ gns
      br >> "Nomyx language files:" >> br
      H.a "Rules examples"    ! (href $ "/src/Language/Nomyx/Examples.hs") >> br
      H.a "Basic rules"       ! (href $ "/src/Language/Nomyx/Rule.hs") >> br
      H.a "Rules definitions" ! (href $ "/src/Language/Nomyx/Definition.hs") >> br
      H.a "Rules types"       ! (href $ "/src/Language/Nomyx/Expression.hs") >> br
      H.a "Voting system"     ! (href $ "/src/Language/Nomyx/Vote.hs") >> br
      mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : modDir </> f)) >> br) fmods
      br >> "Upload new rules file:" >> br
      blazeForm up (uploadLink) ! (A.title $ toValue Help.upload)
      br >> "Settings:" >> br
      when admin $ H.a "Create a new game" ! (href $ toValue newGameLink) >> br
      H.a "Player settings" ! (href $ toValue settingsLink) >> br
      H.a "Advanced" ! (href $ toValue advLink) >> br
      H.a "Logout " ! href (toValue logoutURL) >> br


viewGameName :: Game -> RoutedNomyxServer Html
viewGameName g = do
   let gn = _gameName g
   join <-  showURL (JoinGame gn)
   leave <- showURL (LeaveGame gn)
   view <-  showURL (ViewGame gn)
   ok $ do
      tr $ do
         td ! A.id "gameName" $ string $ (gn ++ "   ")
         td $ H.a "View"  ! (href $ toValue view) ! (A.title $ toValue Help.view)
         td $ H.a "Join"  ! (href $ toValue $ "#openModalJoin" ++ gn) ! (A.title $ toValue Help.join)
         td $ H.a "Leave" ! (href $ toValue leave)
         div ! A.id (toValue $ "openModalJoin" ++ gn) ! A.class_ "modalWindow" $ do
            div $ do
               h2 "Joining the game. Please register in the Agora (see the link) and introduce yourself to the other players! \n \
                   If you do not wich to play, you can just view the game."
               H.a "Join"  ! (href $ toValue join) ! A.class_ "join" ! (A.title $ toValue Help.join)
               H.a "View"  ! (href $ toValue view) ! A.class_ "view" ! (A.title $ toValue Help.view)

nomyxPage :: (TVar Session) -> RoutedNomyxServer Response
nomyxPage ts = do
   pn <- getPlayerNumber ts
   s <- liftIO $ atomically $ readTVar ts
   m <- viewMulti pn s
   name <- liftIO $ getPlayersName pn s
   mainPage' "Welcome to Nomyx!"
            (string $ "Welcome to Nomyx, " ++ name ++ "!")
            (H.div ! A.id "multi" $ m)
            False

nomyxSite :: (TVar Session) -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite tm = setDefault HomePage $ mkSitePI (runRouteT $ routedNomyxCommands tm)

routedNomyxCommands :: (TVar Session) -> PlayerCommand -> RoutedNomyxServer Response
routedNomyxCommands ts (U_AuthProfile auth)  = authenticate ts auth
routedNomyxCommands ts PostAuth              = postAuthenticate ts
routedNomyxCommands ts HomePage              = homePage ts
routedNomyxCommands ts MainPage              = nomyxPage ts
routedNomyxCommands ts (JoinGame game)       = joinGame ts game
routedNomyxCommands ts (LeaveGame game)      = leaveGame ts game
routedNomyxCommands ts (ViewGame game)       = viewGamePlayer ts game
routedNomyxCommands ts NewRule               = newRule ts     >>= return . toResponse
routedNomyxCommands _  NewGame               = newGamePage    >>= return . toResponse
routedNomyxCommands ts SubmitNewGame         = newGamePost ts >>= return . toResponse
routedNomyxCommands ts (DoInput en)          = newInput en ts >>= return . toResponse
routedNomyxCommands ts Upload                = newUpload ts   >>= return . toResponse
routedNomyxCommands ts PSettings             = settings ts    >>= return . toResponse
routedNomyxCommands _  Advanced              = advanced       >>= return . toResponse
routedNomyxCommands ts SubmitPlayerSettings  = newSettings ts >>= return . toResponse


uploadForm :: NomyxForm (FilePath, FilePath, ContentType)
uploadForm = RB.inputFile

newUpload :: (TVar Session) -> RoutedNomyxServer Html
newUpload ts = do
    methodM POST
    pn <- getPlayerNumber ts
    r <- liftRouteT $ eitherForm environment "user" uploadForm
    link <- showURL MainPage
    (T.Session sh _ _) <- liftIO $ readTVarIO ts
    case r of
       (Right (temp,name,_)) -> webCommand ts $ M.inputUpload pn temp name sh
       (Left _) -> liftIO $ putStrLn $ "cannot retrieve form data"
    seeOther link $ string "Redirecting..."


launchWebServer :: (TVar Session) -> Network -> IO ()
launchWebServer tm net = do
   putStrLn $ "Starting web server...\nTo connect, drive your browser to \"" ++ nomyxURL net ++ "/Nomyx\""
   d <- PN.getDataDir
   d' <- PNL.getDataDir
   simpleHTTP nullConf {HS.port = T._port net} $ server d d' tm net

--serving Nomyx web page as well as data from this package and the language library package
server :: FilePath -> FilePath -> (TVar Session) -> Network -> ServerPartT IO Response
server d d' tm net = mconcat [
    serveDirectory DisableBrowsing [] d,
    serveDirectory DisableBrowsing [] d',
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack (nomyxURL net)) "/Nomyx" (nomyxSite tm)
       return $ toResponse html]


