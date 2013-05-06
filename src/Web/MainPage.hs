
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
import Language.Nomyx.Expression
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
import qualified Data.Acid.Advanced as A (query')
default (Integer, Double, Data.Text.Text)


viewMulti :: PlayerNumber -> Session -> RoutedNomyxServer Html
viewMulti pn s = do
   pfd <- A.query' (acidProfileData $ _acid s) (AskProfileData pn)
   gns <- viewGamesTab (map G._game $ _games $ _multi s)
   mgn <- liftRouteT $ lift $ getPlayersGame pn s
   g <- case mgn of
            Just g -> viewGame (G._game g) pn (_pLastRule $ fromJust pfd)
            Nothing -> ok $ h3 "Not in game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ g

viewGamesTab :: [Game] -> RoutedNomyxServer Html
viewGamesTab gs = do
   gns <- mapM viewGameName gs
   newGameLink <- showURL NewGame
   uploadLink <- showURL Upload
   settingsLink <- showURL PSettings
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
      mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : modDir </> f)) >> br) fmods
      br >> "Upload new rules file:" >> br
      blazeForm up (uploadLink) ! (A.title $ toValue Help.upload)
      br >> "Settings:" >> br
      H.a "Create a new game" ! (href $ toValue newGameLink) >> br
      H.a "Player settings" ! (href $ toValue settingsLink) >> br
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
         td $ H.a "View"  ! (href $ toValue view)
         td $ H.a "Join"  ! (href $ toValue join)
         td $ H.a "Leave" ! (href $ toValue leave)

nomyxPage :: (TVar Session) -> RoutedNomyxServer Response
nomyxPage ts = do
   pn <- getPlayerNumber ts
   s <- liftRouteT $ lift $ atomically $ readTVar ts
   m <- viewMulti pn s
   name <- liftRouteT $ lift $ getPlayersName pn s
   mainPage' "Welcome to Nomyx!"
            (string $ "Welcome to Nomyx, " ++ name ++ "!")
            (H.div ! A.id "multi" $ m)
            False

nomyxSite :: (TVar Session) -> Site PlayerCommand (ServerPartT IO Response)
nomyxSite tm = setDefault HomePage $ mkSitePI (runRouteT $ routedNomyxCommands tm)

routedNomyxCommands :: (TVar Session) -> PlayerCommand -> RoutedNomyxServer Response
routedNomyxCommands ts  (U_AuthProfile authProfileURL) = do
   (T.Session _ _ Acid{..}) <- liftRouteT $ lift $ atomically $ readTVar ts
   postPickedURL <- showURL NewPlayer
   nestURL U_AuthProfile $ handleAuthProfile acidAuth acidProfile appTemplate Nothing Nothing postPickedURL authProfileURL
routedNomyxCommands ts NewPlayer             = createNewPlayer ts
routedNomyxCommands ts HomePage              = homePage ts
routedNomyxCommands ts MainPage              = nomyxPage ts
routedNomyxCommands ts (JoinGame game)       = joinGame ts game
routedNomyxCommands ts (LeaveGame game)      = leaveGame ts game
routedNomyxCommands ts (ViewGame game)       = viewGamePlayer ts game
routedNomyxCommands ts NewRule               = newRule ts            >>= return . toResponse
routedNomyxCommands _  NewGame               = newGamePage           >>= return . toResponse
routedNomyxCommands ts SubmitNewGame         = newGamePost ts        >>= return . toResponse
routedNomyxCommands ts (DoInputChoice en)    = newInputChoice en ts  >>= return . toResponse
routedNomyxCommands ts (DoInputString en)    = newInputString en ts  >>= return . toResponse
routedNomyxCommands ts Upload                = newUpload ts          >>= return . toResponse
routedNomyxCommands ts PSettings             = settings ts           >>= return . toResponse
routedNomyxCommands ts SubmitPlayerSettings  = newSettings ts        >>= return . toResponse

{-
nomyxPageComm' :: PlayerNumber -> (TVar Multi) -> StateT Multi IO () -> RoutedNomyxServer Html
nomyxPageComm' pn tm comm = do
    liftRouteT $ lift $ protectedExecCommand tm comm
    nomyxPageServer pn tm

protectedExecCommand :: (TVar Multi) -> StateT Multi IO a -> IO ()
protectedExecCommand tm sm = do
    --liftIO $ mapM_ (uncurry setResourceLimit) limits
    mv <- newEmptyMVar
    before <- atomically $ readTVar tm
    id <- forkIO $ CE.catchJust  (\e -> if isUserError e then Just () else Nothing) (execBlocking sm before mv) (\e-> putStrLn $ show e)
    forkIO $ watchDog' 10 id mv
    getCurrentTime >>= (\a -> putStrLn $ "before takevar " ++ show a)
    res <- takeMVar mv
    case res of
       Nothing -> (atomically $ writeTVar tm before) >> getCurrentTime >>= (\a -> putStrLn $ "writing before" ++ show a)
       Just (_, after) -> (atomically $ writeTVar tm after) >> getCurrentTime >>= (\a -> putStrLn $ "writing after " ++ show a)

watchDog' :: Int -> ThreadId -> MVar (Maybe x) -> IO ()
watchDog' t tid mv = do
   threadDelay $ t * 1000000
   killThread tid
   getCurrentTime >>= (\a -> putStrLn $ "process timeout " ++ show a)
   tryPutMVar mv Nothing
   return ()

execBlocking :: StateT Multi IO a -> Multi -> MVar (Maybe (a, Multi)) -> IO ()
execBlocking sm m mv = do
   hSetBuffering stdout NoBuffering
   getCurrentTime >>= (\a -> putStrLn $ "before runstate " ++ show a)
   res@(_, m') <- runStateT sm m --runStateT (inPlayersGameDo 1 $ liftT $ evalExp (do let (a::Int) = a in outputAll $ show a) 1) m --
   getCurrentTime >>= (\a -> putStrLn $ "after runstate " ++ show a)
   res' <- evaluate res
   putMVar mv (Just res')
-}


uploadForm :: NomyxForm (FilePath, FilePath, ContentType)
uploadForm = RB.inputFile

newUpload :: (TVar Session) -> RoutedNomyxServer Html
newUpload ts = do
    methodM POST
    pn <- getPlayerNumber ts
    r <- liftRouteT $ eitherForm environment "user" uploadForm
    link <- showURL MainPage
    (T.Session sh _ _) <- liftRouteT $ lift $ readTVarIO ts
    case r of
       (Right (path,name,_)) -> webCommand ts $ M.inputUpload pn path name sh
       (Left _) -> liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
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
    serveDirectory EnableBrowsing [] d,
    serveDirectory EnableBrowsing [] d', do
       decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack (nomyxURL net)) "/Nomyx" (nomyxSite tm)
       return $ toResponse html]


