{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module manages multi-player commands.
module Nomyx.Core.Session where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.State
import qualified Data.Acid.Advanced                  as A (query', update')
import           Data.List
import           Data.Maybe
import           Data.Time                           as T
import           Debug.Trace.Helpers
import           Language.Haskell.Interpreter        (InterpreterError)
import           Language.Haskell.Interpreter.Server (ServerHandle)
import           Nomyx.Language
import           Nomyx.Core.Engine                   as G
import           Nomyx.Core.Interpret
import           Nomyx.Core.Multi
import           Nomyx.Core.Profile
import           Nomyx.Core.Serialize
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           System.IO.PlafCompat
import           Imprevu.Evaluation

-- | add a new player
newPlayer :: PlayerNumber -> PlayerSettings -> StateT Session IO ()
newPlayer uid ps = do
   s <- get
   --void $ A.update' (acidAuth $ _profiles s) (SetDefaultSessionTimeout $ 3600 * 24 * 7 *25)
   void $ A.update' (_acidProfiles s) (NewProfileData uid ps)

-- | starts a new game
newGame :: GameName -> GameDesc -> PlayerNumber -> Bool -> StateT Session IO ()
newGame name desc pn isPublic = zoom multi $ newGame' name desc pn isPublic

newGame' :: GameName -> GameDesc -> PlayerNumber -> Bool -> StateT Multi IO ()
newGame' name desc pn isPublic = do
      gs <- use gameInfos
      if not $ any ((== name) . getL gameNameLens) gs then do
         tracePN pn $ "Creating a new game with name: " ++ name
         t <- lift T.getCurrentTime
         -- create a game with zero players
         lg <- lift $ initialGameInfo name desc isPublic (Just pn) t
         void $ gameInfos %= (lg : )
      else tracePN pn "this name is already used"

forkGame :: GameName -> GameName -> GameDesc -> Bool -> PlayerNumber -> StateT Session IO ()
forkGame fromgn newgn desc isPublic pn = zoom multi $ do
   gms <- use gameInfos
   case filter ((== fromgn) . getL gameNameLens) gms of
      [gi] -> do
         tracePN pn $ "Forking game: " ++ fromgn
         time <- liftIO T.getCurrentTime
         let lg = ((game . gameName) .~ newgn) .
                  ((game . gameDesc) .~ desc) $ _loggedGame gi
         let gi' = GameInfo {
            _loggedGame     = lg,
            _ownedBy        = Just pn,
            _forkedFromGame = Just fromgn,
            _isPublic       = isPublic,
            _startedAt      = time}
         void $ gameInfos %= (gi' : )
      _ -> tracePN pn $ "Forking game: no game by that name: " ++ fromgn

-- | join a game (also view it for conveniency)
joinGame :: GameName -> PlayerNumber -> StateT Session IO ()
joinGame gn pn = do
   s <- get
   name <- lift $ Nomyx.Core.Profile.getPlayerName pn s
   inGameDo gn $ G.execGameEvent $ JoinGame pn name

-- | delete a game.
delGame :: GameName -> StateT Session IO ()
delGame name = zoom multi $ void $ gameInfos %= filter ((/= name) . getL gameNameLens)

-- | leave a game.
leaveGame :: GameName -> PlayerNumber -> StateT Session IO ()
leaveGame game pn = inGameDo game $ G.execGameEvent $ LeaveGame pn

-- | insert a rule in pending rules.
submitRule :: RuleTemplate -> PlayerNumber -> GameName -> StateT Session IO ()
submitRule rt pn gn = do
   tracePN pn $ "proposed " ++ show rt
   compileRule rt pn gn Propose "Rule submitted OK! See \"Rules\" tab or \"Inputs/Ouputs\" tab for actions."

adminSubmitRule :: RuleTemplate -> PlayerNumber -> GameName -> StateT Session IO ()
adminSubmitRule rt pn gn = do
   tracePN pn $ "admin proposed " ++ show rt
   compileRule rt pn gn SystemAdd "Admin rule submitted OK!"

checkRule :: RuleTemplate -> PlayerNumber -> GameName -> StateT Session IO ()
checkRule rt pn gn = do
   tracePN pn $ "check rule " ++ show rt
   compileRule rt pn gn Check "Rule compiled OK. Now you can submit it!"

compileRule :: RuleTemplate -> PlayerNumber -> GameName -> RuleEv -> String -> StateT Session IO ()
compileRule rt pn gn re msg = do
   mods <- getModules rt
   mrr <- liftIO $ interpretRule (_rRuleCode rt) mods
   case mrr of
      Right r -> do
         tracePN pn "proposed rule compiled OK "
         inGameDo gn $ G.execGameEvent' (Just $ Right r) (ProposeRuleEv re pn rt mods)
         modifyProfile pn (pLastRule .~ Just (rt, msg))
      Left e -> submitRuleError rt pn gn e

getModules :: RuleTemplate -> StateT Session IO [ModuleInfo]
getModules rt = do
   s <- get
   let mods = _mModules $ _mLibrary $ _multi s
   return $ catMaybes $ map (getModule mods) (_rDeclarations rt)

getModule :: [ModuleInfo] -> FilePath -> Maybe ModuleInfo
getModule ms fp = listToMaybe $ filter (\(ModuleInfo fp' _) -> (fp==fp')) ms

submitRuleError :: RuleTemplate -> PlayerNumber -> GameName -> InterpreterError -> StateT Session IO ()
submitRuleError sr pn gn e = do
   let errorMsg = showInterpreterError e
   inGameDo gn $ execGameEvent $ GLog (Just pn) ("Error in submitted rule: " ++ errorMsg)
   tracePN pn ("Error in submitted rule: " ++ errorMsg)
   modifyProfile pn (pLastRule .~ Just (sr, errorMsg))

newRuleTemplate :: RuleTemplate -> StateT Session IO ()
newRuleTemplate rt = do
  liftIO $ putStrLn $ "Inserted new template: " ++ show rt
  (multi . mLibrary . mTemplates) %= (addRT rt)

updateRuleTemplates :: [RuleTemplate] -> StateT Session IO ()
updateRuleTemplates rts = do
  liftIO $ putStrLn $ (_rAuthor $ head rts) ++ " has updated templates"
  (multi . mLibrary . mTemplates) .= rts

addRT :: RuleTemplate -> [RuleTemplate] -> [RuleTemplate]
addRT rt rts = case (find (\rt' -> (_rName rt) == (_rName rt'))) rts of
  (Just rt') -> replace rt' rt rts
  Nothing -> rt:rts

delRuleTemplate :: GameName -> RuleName -> PlayerNumber -> StateT Session IO ()
delRuleTemplate gn rn pn = do
  tracePN pn $ "del template " ++ show rn
  (multi . mLibrary . mTemplates) %= filter (\rt -> _rName rt /= rn)

updateModules :: [ModuleInfo] -> StateT Session IO ()
updateModules ms = (multi . mLibrary . mModules) .= ms

inputResult :: PlayerNumber -> EventNumber -> Input -> InputData -> GameName -> StateT Session IO ()
inputResult pn en is id gn = inGameDo gn $ execGameEvent $ InputResult pn en is id

-- | upload a rule file, given a player number, the full path of the file, the file name and the server handle
inputUpload :: PlayerNumber -> FilePath -> FilePath -> StateT Session IO Bool
inputUpload pn temp mod = undefined
--do
--   sd <- use (multi . mSettings . saveDir)
--   m <- liftIO $ loadModule temp mod sh sd
--   tracePN pn $ " uploaded " ++ show mod
--   case m of
--      Nothing -> do
--         inAllGamesDo $ execGameEvent $ GLog (Just pn) ("File loaded: " ++ show temp ++ ", as " ++ show mod ++"\n")
--         tracePN pn "upload success"
--         modifyProfile pn (pLastUpload .~ UploadSuccess)
--         return True
--      Just e -> do
--         let errorMsg = showInterpreterError e
--         inAllGamesDo $ execGameEvent $ GLog (Just pn) ("Error in file: " ++ show e ++ "\n")
--         tracePN pn $ "upload failed: \n" ++ show e
--         modifyProfile pn (pLastUpload .~ UploadFailure (temp, errorMsg))
--         return False

-- | update player settings
playerSettings :: PlayerSettings -> PlayerNumber -> StateT Session IO ()
playerSettings playerSettings pn = modifyProfile pn (pPlayerSettings .~ playerSettings)

playAs :: Maybe PlayerNumber -> PlayerNumber -> GameName -> StateT Session IO ()
playAs playAs pn g = inGameDo g $ do
   pls <- use (game . players)
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> tracePN pn "player not in game"
      Just pi -> (game . players) .= replaceWith ((== pn) . getL playerNumber) (pi {_playAs = playAs}) pls

adminPass :: String -> PlayerNumber -> StateT Session IO ()
adminPass pass pn = do
   s <- get
   if pass == (_adminPassword $ _mSettings $ _multi s) then do
      tracePN pn "getting admin rights"
      modifyProfile pn $ pIsAdmin .~ True
   else do
      tracePN pn "submitted wrong admin password"
      modifyProfile pn $ pIsAdmin .~ False

globalSettings :: Bool -> StateT Session IO ()
globalSettings mails = (multi . mSettings . sendMails) .= mails

-- | Utility functions

getNewPlayerNumber :: StateT Session IO PlayerNumber
getNewPlayerNumber = do
   s <- get
   pfd <- A.query' (_acidProfiles s) AskProfileDataNumber
   return $ pfd + 1

-- | this function apply the given game actions to the game the player is in.
inAllGamesDo :: StateT LoggedGame IO a -> StateT Session IO ()
inAllGamesDo action = do
   t <- lift T.getCurrentTime
   gis <- use (multi . gameInfos)
   forM_ gis $ \gi -> do
         (_, mylg) <- lift $ runStateT action (set (game . currentTime) t (_loggedGame gi))
         zoom multi $ modifyGame (gi {_loggedGame = mylg})

inGameDo :: GameName -> StateT LoggedGame IO () -> StateT Session IO ()
inGameDo gn action = zoom multi $ do
   (gs :: [GameInfo]) <- use gameInfos
   case find ((==gn) . getL gameNameLens) gs of
      Nothing -> traceM "No game by that name"
      Just (gi::GameInfo) -> do
         t <- lift T.getCurrentTime
         mylg <- lift $ execWithGame' t action (_loggedGame gi)
         modifyGame (gi {_loggedGame = mylg})

-- update a session by executing a command.
-- we set a watchdog in case the evaluation would not finish
updateSession :: TVar Session -> StateT Session IO () -> IO ()
updateSession ts sm = do
   s <- atomically $ readTVar ts
   let delay = _watchdog $ _mSettings $ _multi s
   ms <- evalWithWatchdog delay s (evalSession sm)
   case ms of
      Just s' -> do
         atomically $ writeTVar ts s'
         save $ _multi s
      Nothing -> putStrLn "thread timed out, session discarded"

evalSession :: StateT Session IO () -> Session -> IO Session
evalSession sm s = do
   s' <- execStateT sm s
   writeFile nullFileName $ show $ _multi s' --dirty hack to force deep evaluation --deepseq (_multi s') (return ())
   return s'

