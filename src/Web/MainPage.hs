
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables, NamedFieldPuns, Rank2Types, DoAndIfThenElse, StandaloneDeriving, OverloadedStrings, ExtendedDefaultRules#-}

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
import Paths_Nomyx_Rules as PNR
import Control.Monad.State
import Data.Monoid
import Control.Concurrent.STM
import Language.Nomyx.Expression
import Text.Reform.Happstack
import Text.Reform
import Happstack.Server
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Web.Help as Help
import Network.BSD
import Types
import Serialize
import Multi --TODO to remove
import Web.Game
import Web.Common
import Web.Login
import qualified Text.Reform.Blaze.String as RB
import qualified Text.Reform.Blaze.Common as RBC
import Control.Applicative
import Data.Text(Text, pack)
default (Integer, Double, Data.Text.Text)

data NewGameForm = NewGameForm String


viewMulti :: PlayerNumber -> Multi -> RoutedNomyxServer Html
viewMulti pn m = do
   gns <- viewGamesTab pn (games m)
   g <- case getPlayersGame pn m of
            Just g -> viewGame g pn
            Nothing -> ok $ h3 "Not in game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ g

viewGamesTab :: PlayerNumber -> [Game] -> RoutedNomyxServer Html
viewGamesTab pn gs = do
   gns <- mapM (viewGameName pn) gs
   newGameLink <- showURL (NewGame pn)
   ng  <- lift $ viewForm "user" $ newGameForm
   uploadLink <- showURL (Upload pn)
   up  <- lift $ viewForm "user" uploadForm
   dd <- lift $ lift $ PN.getDataDir
   mods <- lift $ lift $ getDirectoryContents $ dd </> modDir
   fmods <- lift $ lift $ filterM (getFileStatus . (\f -> joinPath [dd, modDir, f]) >=> return . isRegularFile) $ mods
   ok $ do
      h3 "Games:"
      table $ do
         case gs of
            [] -> tr $ td "No Games"
            _ ->  sequence_ gns
      br >> "Create a new game:" >> br
      blazeForm ng (newGameLink)
      br >> "Rule language files:" >> br
      H.a "Rules examples" ! (href $ "/examples/Examples.hs") >> br
      H.a "Rules definitions" ! (href $ "/src/Language/Nomyx/Rule.hs") >> br
      H.a "Rules types" ! (href $ "/src/Language/Nomyx/Expression.hs") >> br
      mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : modDir </> f)) >> br) fmods
      br >> "Upload new rules file:" >> br
      blazeForm up (uploadLink) ! (A.title $ toValue Help.upload)

viewGameName :: PlayerNumber -> Game -> RoutedNomyxServer Html
viewGameName pn g = do
   let gn = gameName g
   join <- showURL (JoinGame pn gn)
   leave <- showURL (LeaveGame pn)
   --subscribe <- showURL (SubscribeGame pn gn)
   unsubscribe <- showURL (UnsubscribeGame pn gn)
   ok $ do
      tr $ do
         td $ string $ gn
         td $ H.a "Join" ! (href $ toValue join)
         td $ H.a "Leave" ! (href $ toValue leave)
         --td $ H.a "Subscribe" ! (href $ toValue subscribe)
         td $ H.a "Unsubscribe" ! (href $ toValue unsubscribe)


newGameForm :: NomyxForm NewGameForm
newGameForm = pure NewGameForm <*> (RB.inputText "") `RBC.setAttr` A.placeholder "Enter game name"


nomyxPage :: Multi -> PlayerNumber -> RoutedNomyxServer Html
nomyxPage multi pn = do
   m <- viewMulti pn multi
   ok $ do
      H.html $ do
        H.head $ do
          H.title "Welcome to Nomyx!"
          H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomyx.css"
          H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
          H.meta ! A.name "keywords" ! A.content "Nomyx, game, rules, Haskell, auto-reference"
          --H.meta ! A.httpEquiv "refresh" ! A.content "3"
        H.body $ do
          H.div ! A.id "container" $ do
             H.div ! A.id "header" $ string $ "Welcome to Nomyx, " ++ (getPlayersName pn multi) ++ "!"
             H.div ! A.id "multi" $ m



nomyxSite :: (TVar Multi) -> Site PlayerCommand (ServerPartT IO Html)
nomyxSite tm = setDefault Login $ mkSitePI (runRouteT $ routedNomyxCommands tm)

routedNomyxCommands :: (TVar Multi) -> PlayerCommand -> RoutedNomyxServer Html
routedNomyxCommands _  (Login)                     = loginPage
routedNomyxCommands tm (PostLogin)                 = postLogin tm
routedNomyxCommands tm (Noop pn)                   = nomyxPageComm pn tm (return ())
routedNomyxCommands tm (JoinGame pn game)          = nomyxPageComm pn tm (update $ MultiJoinGame game pn)
routedNomyxCommands tm (LeaveGame pn)              = nomyxPageComm pn tm (update $ MultiLeaveGame pn)
routedNomyxCommands tm (SubscribeGame pn game)     = nomyxPageComm pn tm (update $ MultiSubscribeGame game pn)
routedNomyxCommands tm (UnsubscribeGame pn game)   = nomyxPageComm pn tm (update $ MultiUnsubscribeGame game pn)
routedNomyxCommands tm (NewRule pn)                = newRule pn tm
routedNomyxCommands tm (NewGame pn)                = newGameWeb pn tm
routedNomyxCommands tm (DoInputChoice pn en)       = newInputChoice pn en tm
routedNomyxCommands tm (DoInputString pn en)       = newInputString pn en tm
routedNomyxCommands tm (Upload pn)                 = newUpload pn tm

--execute the given instructions (Comm) and embed the result in a web page
nomyxPageComm :: PlayerNumber -> (TVar Multi) -> StateT Multi IO () -> RoutedNomyxServer Html
nomyxPageComm pn tm comm = execCommand tm comm >> nomyxPageServer pn tm

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
newRule :: PlayerNumber -> (TVar Multi) -> RoutedNomyxServer Html
newRule pn tm = do
   methodM POST
   r <- liftRouteT $ eitherForm environment "user" newRuleForm
   link <- showURL $ Noop pn
   case r of
       Right (NewRuleForm name text code) -> do
         --debugM ("Rule submitted: name =" ++ name ++ "\ntext=" ++ text ++ "\ncode=" ++ code ++ "\npn=" ++ (show pn))
         nomyxPageComm pn tm (update $ MultiSubmitRule name text code pn)
       (Left _) -> do
          liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."

newGameWeb :: PlayerNumber -> (TVar Multi) -> RoutedNomyxServer Html
newGameWeb pn tm = do
   methodM POST
   r <- liftRouteT $ eitherForm environment "user" newGameForm
   case r of
      Left _                      -> error $ "error: newGame"
      Right (NewGameForm name) -> nomyxPageComm pn tm (update $ MultiNewGame name pn)


newUpload :: PlayerNumber -> (TVar Multi) -> RoutedNomyxServer Html
newUpload pn tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" uploadForm
    link <- showURL $ Noop pn
    case r of
       (Right (path,name,content)) -> do
          lift $ lift $ putStrLn $ "Upload entered:" ++ (show path) ++ " " ++ (show name) ++ " " ++ (show content)
          execCommand tm $ update $ MultiInputUpload pn path name
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."

nomyxPageServer :: PlayerNumber -> (TVar Multi) -> RoutedNomyxServer Html
nomyxPageServer pn tm = do
   multi <- liftRouteT $ lift $ atomically $ readTVar tm
   nomyxPage multi pn


launchWebServer :: (TVar Multi) -> HostName -> Port -> IO ()
launchWebServer tm host portNumber = do
   putStrLn $ "Starting web server...\nTo connect, drive your browser to \"http://" ++ host ++ ":" ++ (show portNumber) ++ "/Login\""
   d <- PN.getDataDir
   d' <- PNR.getDataDir
   simpleHTTP nullConf {port=portNumber} $ server d d' tm host portNumber

--serving Nomyx web page as well as data from this package and the language library package
server :: FilePath -> FilePath -> (TVar Multi) -> HostName -> Port -> NomyxServer Response
server d d' tm host port = mconcat [
    serveDirectory EnableBrowsing [] d,
    serveDirectory EnableBrowsing [] d', do
       decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack ("http://" ++ host ++ ":" ++ (show port))) "/Login" (nomyxSite tm)
       return $ toResponse html]




uploadForm :: NomyxForm (FilePath, FilePath, ContentType)
uploadForm = RB.inputFile



