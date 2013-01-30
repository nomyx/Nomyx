
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables, NamedFieldPuns, Rank2Types, DoAndIfThenElse, StandaloneDeriving, OverloadedStrings#-}

module Web (launchWebServer) where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.Site
import Web.Routes.PathInfo
import Web.Routes.Happstack
import Web.Routes.RouteT
import Web.Routes.TH (derivePathInfo)
import Text.Blaze.Internal
import Game
--import Multi
import Control.Monad
import Paths_Nomyx as PN
import Paths_Nomyx_Rules as PNR
import Control.Monad.State
import Data.Monoid
import Data.String
import Control.Concurrent.STM
import Language.Haskell.Interpreter.Server
import Language.Nomyx.Expression
import Language.Nomyx.Evaluation
import Language.Nomyx.Rule hiding (getCurrentTime)
import Utils
import Data.Maybe
import Text.Reform.Happstack
import Text.Reform
import Forms
import Data.Text hiding (concat, map, filter, concatMap, length, intersperse)
import Happstack.Server
import Data.List
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Help
import Network.BSD
import Mueval.Resources
import System.Posix.Resource
import Control.Concurrent
import Mueval.Parallel
import System.Posix.Signals (sigXCPU, installHandler, Handler(CatchOnce))
import Control.Exception.Extensible (ErrorCall(..),SomeException,catch)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Time
import Control.Exception as CE
import System.IO.Error
import Types
import Serialize
import Multi --TODO to remove

-- | associate a player number with a handle
data PlayerClient = PlayerClient PlayerNumber deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server [PlayerClient] deriving (Eq, Show)
type Port = Int

data PlayerCommand = Login
                   | PostLogin
                   | Noop            PlayerNumber
                   | JoinGame        PlayerNumber GameName
                   | LeaveGame       PlayerNumber
                   | SubscribeGame   PlayerNumber GameName
                   | UnsubscribeGame PlayerNumber GameName
                   | DoInputChoice   PlayerNumber EventNumber
                   | DoInputString   PlayerNumber String
                   | NewRule
                   | NewGame
                   | Upload PlayerNumber
                   deriving (Show)

$(derivePathInfo ''PlayerCommand)

instance PathInfo Bool where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "bool") (checkBool . show)
   where checkBool str =
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing

modDir = "modules"

type NomyxServer       = ServerPartT IO
type RoutedNomyxServer = RouteT PlayerCommand NomyxServer


nomyxSite :: (TVar Multi) -> Site PlayerCommand (ServerPartT IO Html)
nomyxSite tm = setDefault Login $ mkSitePI (runRouteT $ routedNomyxCommands tm)

viewGame :: Game -> PlayerNumber -> RoutedNomyxServer Html
viewGame g pn = do
   rf <- ruleForm pn
   vi <- viewInputs pn $ events g
   ok $ table $ do
      td ! A.id "gameCol" $ do
         table $ do
            tr $ td $ h3 $ string $ "Viewing game: " ++ gameName g  --div ! A.id "gameName"
            tr $ td $ viewPlayers $ players g -- div ! A.id "citizens"
            tr $ td $ viewVictory g --  div ! A.id "victory"
      td ! A.id "gameElem" $ do
         table $ do
         tr $ td $ div ! A.id "rules" $ viewAllRules g
         tr $ td $ div ! A.id "inputs" ! A.title (toValue Help.inputs) $ vi
         tr $ td $ div ! A.id "events" ! A.title (toValue Help.events) $ viewEvents $ events g
         tr $ td $ div ! A.id "variables" ! A.title (toValue Help.variables)$ viewVars $ variables g
         tr $ td $ div ! A.id "newRule" $ rf
         tr $ td $ div ! A.id "outputs" ! A.title (toValue Help.outputs)$ viewOutput (outputs g) pn

viewPlayers :: [PlayerInfo] -> Html
viewPlayers pis = do
   h5 "Players in game:"
   table $ mapM_ viewPlayer (sort pis)


viewPlayer :: PlayerInfo -> Html
viewPlayer pi = tr $ do
    td $ string $ show $ playerNumber pi
    td $ string $ playerName pi

viewVictory :: Game -> Html
viewVictory g = do
    let vs = mapMaybe (getPlayersNameMay g) (victory g)
    case vs of
        []   -> br
        a:[] -> h3 $ string $ "Player " ++ (show a) ++ " won the game!"
        a:bs -> h3 $ string $ "Players " ++ (concat $ intersperse ", " $ bs) ++ " and " ++ a ++ " won the game!"

viewAllRules :: Game -> Html
viewAllRules g = do
   h3 "Rules"
   viewRules "Active rules:" (activeRules g) ! (A.title $ toValue Help.actives)
   viewRules "Pending rules:" (pendingRules g) ! (A.title $ toValue Help.pendings)
   viewRules "Suppressed rules:" $ rejectedRules g

viewRules :: Html -> [Rule] -> Html
viewRules _ [] = return ()
viewRules title nrs = do
   table ! A.class_ "table" $ do
      caption $ h4 title
      thead $ do
         td ! A.class_ "td" $ text "Number"
         td ! A.class_ "td" $ text "Name"
         td ! A.class_ "td" $ text "Description"
         td ! A.class_ "td" $ text "Proposed by"
         td ! A.class_ "td" $ text "Code of the rule"
         td ! A.class_ "td" $ text "Assessed by"
      forM_ nrs viewRule

viewRule :: Rule -> Html
viewRule nr = tr $ do
   td ! A.class_ "td" $ string . show $ rNumber nr
   td ! A.class_ "td" $ string $ rName nr
   td ! A.class_ "td" $ string $ rDescription nr
   td ! A.class_ "td" $ string $ if rProposedBy nr == 0 then "System" else "Player " ++ (show $ rProposedBy nr)
   td ! A.class_ "td" $ string $ rRuleCode nr
   td ! A.class_ "td" $ string $ case rAssessedBy nr of
      Nothing -> "Not assessed"
      Just 0  -> "System"
      Just a  -> "Rule " ++ (show $ a)

viewEvents :: [EventHandler] -> Html
viewEvents [] = h3 "Events" >> h5 "No Events"
viewEvents ehs = do
   h3 "Events"
   table ! A.class_ "table" $ do
      thead $ do
         td ! A.class_ "td" $ text "Event Number"
         td ! A.class_ "td" $ text "By Rule"
         td ! A.class_ "td" $ text "Event"
      mapM_ viewEvent $ sort ehs

viewEvent :: EventHandler -> Html
viewEvent (EH eventNumber ruleNumber event _) = tr $ do
   td ! A.class_ "td" $ string . show $ eventNumber
   td ! A.class_ "td" $ string . show $ ruleNumber
   td ! A.class_ "td" $ string . show $ event

viewInputs :: PlayerNumber -> [EventHandler] -> RoutedNomyxServer Html
viewInputs pn ehs = do
   mis <- mapM (viewInput pn) $ sort ehs
   let is = catMaybes mis
   case length is of
      0 -> ok $ h3 "Inputs" >> h5 "No Inputs"
      _ -> ok $ do
         h3 "Inputs"
         table $ do
            mconcat is

viewInput :: PlayerNumber -> EventHandler -> RoutedNomyxServer (Maybe Html)
viewInput me (EH eventNumber _ (InputChoice pn title choices def) _) | me == pn = do
    link <- showURL (DoInputChoice pn eventNumber)
    lf  <- lift $ viewForm "user" $ inputChoiceForm title (map show choices) (show def)
    return $ Just $ tr $ td $ blazeForm lf (link)
viewInput me (EH _ _ (InputString pn title) _) | me == pn = do
    link <- showURL (DoInputString pn title)
    lf  <- lift $ viewForm "user" $ inputStringForm title
    return $ Just $ tr $ td $ blazeForm lf (link)
viewInput _ _ = return Nothing

viewVars :: [Var] -> Html
viewVars [] = h3 "Variables" >> h5 "No Variables"
viewVars vs = do
   h3 "Variables"
   table ! A.class_ "table" $ do
      thead $ do
         td ! A.class_ "td" $ text "Rule number"
         td ! A.class_ "td" $ text "Name"
         td ! A.class_ "td" $ text "Value"
      mapM_ viewVar vs

viewVar :: Var -> Html
viewVar (Var vRuleNumber vName vData) = tr $ do
   td ! A.class_ "td" $ string . show $ vRuleNumber
   td ! A.class_ "td" $ string . show $ vName
   td ! A.class_ "td" $ string . show $ vData


ruleForm :: PlayerNumber -> RoutedNomyxServer Html
ruleForm pn = do
   link <- showURL NewRule
   ok $ do
      h3 "Propose a new rule:"
      H.form ! A.method "POST" ! A.action (toValue link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! A.for "name" $ "Name: "
      input ! type_ "text" ! name "name" ! A.id "name" ! tabindex "1" ! accesskey "N"
      H.label ! A.for "text" $ "      Short description: "
      input ! type_ "text" ! name "description" ! A.id "description" ! tabindex "2" ! accesskey "T"
      H.br
      H.label ! A.for "text" $ "Code: "
      textarea ! name "code" ! A.id "code" ! tabindex "3" ! accesskey "C" ! A.placeholder "Enter here your rule" !
       A.title (toValue Help.code) $ ""
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "4" ! accesskey "S" ! value "Submit rule!"

viewOutput :: [Output] -> PlayerNumber -> Html
viewOutput [] _ = h3 "Output" >> h5 "No Output"
viewOutput os pn = do
   h3 "Output"
   let myos = map snd $ filter (\o -> fst o == pn) os
   mapM_ viewMessages [myos]

viewMessages :: [String] -> Html
viewMessages = mapM_ (\s -> string s >> br)


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
   ng <- newGameForm pn
   link <- showURL (Upload pn)
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
      br >> "Create a new game:"
      ng
      br >> "Rule language files:" >> br
      H.a "Rules examples" ! (href $ "/examples/Examples.hs") >> br
      H.a "Rules definitions" ! (href $ "/src/Language/Nomyx/Rule.hs") >> br
      H.a "Rules types" ! (href $ "/src/Language/Nomyx/Expression.hs") >> br
      mapM_ (\f -> (H.a $ toHtml f ) ! (href $ toValue (pathSeparator : modDir </> f)) >> br) fmods
      br >> "Upload new rules file:" >> br
      blazeForm up (link) ! (A.title $ toValue Help.upload)

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

newGameForm :: PlayerNumber -> RoutedNomyxServer Html
newGameForm pn = do
   link <- showURL NewGame
   ok $ H.form ! A.method "POST" ! A.action (toValue link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
      input ! type_ "text" ! name "name" ! A.id "name" ! tabindex "1" ! accesskey "G" ! A.title "Game name"
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "2" ! accesskey "S" ! value "Create New Game!"


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


loginPage :: RoutedNomyxServer Html
loginPage = do
   link <- showURL PostLogin
   lf  <- lift $ viewForm "user" loginForm
   ok $ H.html $ do
      H.head $ do
        H.title "Login to Nomyx"
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomyx.css"
        H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomyx, game, rules, Haskell, auto-reference"
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ "Login to Nomyx"
           H.div ! A.id "login" $ blazeForm lf (link)
           H.div ! A.id "footer" $ string "Copyright Corentin Dupont 2012"


routedNomyxCommands :: (TVar Multi) -> PlayerCommand -> RoutedNomyxServer Html
routedNomyxCommands _  (Login)                     = loginPage
routedNomyxCommands tm (PostLogin)                 = postLogin tm
routedNomyxCommands tm (Noop pn)                   = nomyxPageComm pn tm (return ())
routedNomyxCommands tm (JoinGame pn game)          = nomyxPageComm pn tm (update $ MultiJoinGame game pn)
routedNomyxCommands tm (LeaveGame pn)              = nomyxPageComm pn tm (update $ MultiLeaveGame pn)
routedNomyxCommands tm (SubscribeGame pn game)     = nomyxPageComm pn tm (update $ MultiSubscribeGame game pn)
routedNomyxCommands tm (UnsubscribeGame pn game)   = nomyxPageComm pn tm (update $ MultiUnsubscribeGame game pn)
routedNomyxCommands tm (NewRule)                   = newRule tm
routedNomyxCommands tm (NewGame)                   = newGameWeb tm
routedNomyxCommands tm (DoInputChoice pn en)       = newInputChoice pn en tm
routedNomyxCommands tm (DoInputString pn en)       = newInputString pn en tm
routedNomyxCommands tm (Upload pn)                 = newUpload pn tm

--execute the given instructions (Comm) and embed the result in a web page
nomyxPageComm :: PlayerNumber -> (TVar Multi) -> StateT Multi IO () -> RoutedNomyxServer Html
nomyxPageComm pn tm comm = execCommand tm comm >> nomyxPageServer pn tm

execCommand :: (TVar Multi) -> StateT Multi IO a -> RoutedNomyxServer a
execCommand tm sm = do
    m <- liftRouteT $ lift $ atomically $ readTVar tm
    (a, m') <- liftRouteT $ lift $ runStateT sm m
    liftRouteT $ lift $ atomically $ writeTVar tm m'
    return a


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

newRule :: (TVar Multi) -> RoutedNomyxServer Html
newRule tm = do
   methodM POST -- only accept a post method
   mbEntry <- getData -- get the data
   case mbEntry of
      Left a -> error $ "error: newRule " ++ (concat a)
      Right (NewRuleForm name text code pn) -> do
         --debugM ("Rule submitted: name =" ++ name ++ "\ntext=" ++ text ++ "\ncode=" ++ code ++ "\npn=" ++ (show pn))
         nomyxPageComm pn tm (update $ MultiSubmitRule name text code pn)


newGameWeb :: (TVar Multi) -> RoutedNomyxServer Html
newGameWeb tm = do
   methodM POST
   mbEntry <- getData
   case mbEntry of
      Left a                      -> error $ "error: newGame" ++ (concat a)
      Right (NewGameForm name pn) -> nomyxPageComm pn tm (update $ MultiNewGame name pn)

newInputChoice :: PlayerNumber -> EventNumber -> (TVar Multi) -> RoutedNomyxServer Html
newInputChoice pn en tm = do
    multi <- liftRouteT $ lift $ atomically $ readTVar tm
    let mg = fromJust $ getPlayersGame pn multi
    let eventHandler = fromJust $ findEvent en (events mg)
    methodM POST
    let (title, choices, def) = getChoices eventHandler
    r <- liftRouteT $ eitherForm environment "user" (inputChoiceForm title choices def)
    link <- showURL $ Noop pn
    case r of
       (Right c) -> do
          liftRouteT $ lift $ putStrLn $ "choice:" ++ (show c)
          execCommand tm $ update $ MultiInputChoiceResult en c pn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."

getChoices :: EventHandler -> (String, [String], String)
getChoices (EH _ _ (InputChoice _ title choices def) _) = (title, map show choices, show def)
getChoices _ = error "InputChoice event expected"

newInputString :: PlayerNumber -> String -> (TVar Multi) -> RoutedNomyxServer Html
newInputString pn title tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" (inputStringForm title)
    link <- showURL $ Noop pn
    case r of
       (Right c) -> do
          liftRouteT $ lift $ putStrLn $ "entered:" ++ (show c)
          execCommand tm $ update $ MultiInputStringResult title c pn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."

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


postLogin :: (TVar Multi) -> RoutedNomyxServer Html
postLogin tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" loginForm
    case r of
       (Right (LoginPass login password)) -> do
          liftRouteT $ lift $ putStrLn $ "login:" ++ login
          liftRouteT $ lift $ putStrLn $ "password:" ++ password
          mpn <- execCommand tm $ newPlayerWeb login password
          case mpn of
             Just pn -> do
                link <- showURL $ Noop pn
                seeOther link $ string "Redirecting..."
             _ -> error "cannot login"
       (Left _) -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."


newPlayerWeb :: PlayerName -> PlayerPassword -> StateT Multi IO (Maybe PlayerNumber)
newPlayerWeb name pwd = do
   --find that name among the list
   mpn <- findPlayer name
   case mpn of
      Just pl -> do
         say $ "Trying name:" ++ mPlayerName pl
         case pwd == mPassword pl of
            True -> do
               say "password OK"
               return $ Just $ mPlayerNumber pl
            False -> do
               say "password false"
               return Nothing
      Nothing -> do
         say "New player"
         --add the new player to the list
         pn <- getNewPlayerNumber
         update $ MultiNewPlayer PlayerMulti { mPlayerNumber = pn, mPlayerName = name, mPassword = pwd, inGame = Nothing, mMail = ""}
         return (Just pn)


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

instance FromData NewRuleForm where
  fromData = do
    name  <- look "name" `mplus` (error "need rule name")
    text <-  look "description" `mplus` (error "need rule text")
    code <-  look "code" `mplus` (error "need rule code")
    pn <- lookRead "pn" `mplus` (error "need player number")
    return $ NewRuleForm name text code pn

instance FromData NewGameForm where
  fromData = do
    name  <- look "name" `mplus` (error "need rule name")
    pn <- lookRead "pn" `mplus` (error "need player number")
    return $ NewGameForm name pn

