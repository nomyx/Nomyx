{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module manages multi-player commands.
module Nomyx.Core.Session (
  -- * Player managament
  newPlayer,
  -- * Game management
  newGame, newGame',
  forkGame,
  joinGame,
  delGame,
  leaveGame,
  -- * Rule management
  submitRule,
  adminSubmitRule,
  checkRule,
  -- * Library management
  newRuleTemplate,
  addRuleTemplate,
  delRuleTemplate,
  updateLibrary,
  -- * IO
  inputResult,
  applyTimeEvent,
  -- * settings
  playerSettings,
  adminPass,
  globalSettings,
  playAs,
  -- * Session
  updateSession,
  evalSession,
  )
where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.State
import qualified Data.Acid.Advanced                  as A (query', update')
import           Data.List
import           Data.Maybe
import           Data.Time                           as T
import           Debug.Trace.Helpers
import           Language.Haskell.Interpreter        (InterpreterError)
import           Nomyx.Language
import           Nomyx.Core.Engine                   as G
import           Nomyx.Core.Engine.Interpret
import           Nomyx.Core.Multi
import           Nomyx.Core.Profile
import           Nomyx.Core.Serialize
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           Nomyx.Core.Mail
import           System.IO.PlafCompat
import           System.Log.Logger
import           Imprevu.Evaluation 

-- | add a new player
newPlayer :: PlayerNumber -> PlayerSettings -> StateT Session IO ()
newPlayer uid ps = do
   info uid $ "New player: " ++ (_pPlayerName ps)
   s <- get
   --void $ A.update' (acidAuth $ _profiles s) (SetDefaultSessionTimeout $ 3600 * 24 * 7 *25)
   void $ A.update' (_acidProfiles s) (NewProfileData uid ps (_mLibrary $ _multi s))

-- | starts a new game
newGame :: GameName -> GameDesc -> PlayerNumber -> Bool -> StateT Session IO ()
newGame name desc pn isPublic = zoom multi $ newGame' name desc pn isPublic

newGame' :: GameName -> GameDesc -> PlayerNumber -> Bool -> StateT Multi IO ()
newGame' name desc pn isPublic = do
      gs <- use gameInfos
      if not $ any ((== name) . view gameNameLens) gs then do
         info pn $ " creating a new game with name: " ++ name
         t <- lift T.getCurrentTime
         -- create a game with zero players
         lg <- lift $ initialGameInfo name desc isPublic (Just pn) t
         void $ gameInfos %= (lg : )
      else warn pn $ "this name is already used"

forkGame :: GameName -> GameName -> GameDesc -> Bool -> PlayerNumber -> StateT Session IO ()
forkGame fromgn newgn desc isPublic pn = zoom multi $ do
   gms <- use gameInfos
   case filter ((== fromgn) . view gameNameLens) gms of
      [gi] -> do
         info pn $ "Forking game: " ++ fromgn
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
      _ -> warn pn $ "Forking game: no game by that name: " ++ fromgn

-- | join a game (also view it for conveniency)
joinGame :: GameName -> PlayerNumber -> StateT Session IO ()
joinGame gn pn = do
   info pn $ "joining game: " ++ gn
   s <- get
   name <- lift $ Nomyx.Core.Profile.getPlayerName pn s
   inGameDo gn $ G.execGameEvent $ JoinGame pn name

-- | delete a game.
delGame :: GameName -> StateT Session IO ()
delGame gn = do
   info 0 $ "deleting game: " ++ gn
   zoom multi $ void $ gameInfos %= filter ((/= gn) . view gameNameLens)

-- | leave a game.
leaveGame :: GameName -> PlayerNumber -> StateT Session IO ()
leaveGame game pn = do
   info pn $ "left game " ++ game
   inGameDo game $ G.execGameEvent $ LeaveGame pn

-- | insert a rule in pending rules.
submitRule :: RuleTemplate -> PlayerNumber -> GameName -> StateT Session IO ()
submitRule rt pn gn = do
   info pn $ "proposed " ++ show rt
   compileOK <- compileRule rt pn gn Propose "Rule submitted OK! See \"Constitution\" or \"Actions\" tabs for actions."
   when compileOK $ do
      s <- get
      liftIO $ sendMailsSubmitRule s rt pn gn

adminSubmitRule :: RuleTemplate -> PlayerNumber -> GameName -> StateT Session IO ()
adminSubmitRule rt pn gn = do
   info pn $ "admin proposed " ++ show rt
   void $ compileRule rt pn gn SystemAdd "Admin rule submitted OK!"

checkRule :: RuleTemplate -> PlayerNumber -> GameName -> StateT Session IO ()
checkRule rt pn gn = do
   info pn $ "check rule " ++ show rt
   void $ compileRule rt pn gn Check "Rule compiled OK. Now you can submit it!"

compileRule :: RuleTemplate -> PlayerNumber -> GameName -> RuleEv -> String -> StateT Session IO Bool
compileRule rt pn gn re msg = do
   mods <- getModules pn rt
   mrr <- liftIO $ interpretRule (_rRuleCode rt) mods
   case mrr of
      Right r -> do
         info pn "proposed rule compiled OK "
         inGameDo gn $ G.execGameEvent' (Just r) (ProposeRuleEv re pn rt mods)
         modifyProfile pn (pLastRule .~ Just (rt, msg))
         return True
      Left e -> do
         submitRuleError rt pn gn e
         return False

getModules :: PlayerNumber -> RuleTemplate -> StateT Session IO [ModuleInfo]
getModules pn rt = do
   s <- get
   prof <- fromJust <$> getProfile s pn 
   let mods = _mModules $ _pLibrary prof
   return $ catMaybes $ map (getModule mods) (_rDeclarations rt)

getModule :: [ModuleInfo] -> FilePath -> Maybe ModuleInfo
getModule ms fp = listToMaybe $ filter (\(ModuleInfo fp' _) -> (fp==fp')) ms

submitRuleError :: RuleTemplate -> PlayerNumber -> GameName -> InterpreterError -> StateT Session IO ()
submitRuleError sr pn gn e = do
   let errorMsg = showInterpreterError e
   inGameDo gn $ execGameEvent $ GLog (Just pn) ("Error in submitted rule: " ++ errorMsg)
   warn pn ("Error in submitted rule: " ++ errorMsg)
   modifyProfile pn (pLastRule .~ Just (sr, errorMsg))

newRuleTemplate :: PlayerNumber -> RuleTemplate -> StateT Session IO ()
newRuleTemplate pn rt = do
  info pn " inserted new template"
  modifyProfile pn $ (pLibrary . mTemplates) %~ (addRuleTemplate rt)

updateLibrary :: PlayerNumber -> Library -> StateT Session IO ()
updateLibrary pn lib = do
  info pn " updated library"
  modifyProfile pn $ pLibrary .~ lib

addRuleTemplate :: RuleTemplate -> [RuleTemplate] -> [RuleTemplate]
addRuleTemplate rt rts = case (find (\rt' -> (_rName rt) == (_rName rt'))) rts of
  (Just rt') -> replace rt' rt rts
  Nothing -> rt:rts

delRuleTemplate :: RuleName -> PlayerNumber -> StateT Session IO ()
delRuleTemplate rn pn = do
  info pn $ "del template " ++ show rn
  modifyProfile pn $ (pLibrary . mTemplates) %~ filter (\rt -> _rName rt /= rn)

inputResult :: PlayerNumber -> EventNumber -> Input -> InputData -> GameName -> StateT Session IO ()
inputResult pn en is id gn = inGameDo gn $ execGameEvent $ InputResult pn en is id

applyTimeEvent :: UTCTime -> Game -> StateT Session IO () 
applyTimeEvent t g = do
   let ts = G.getTimeEvents t g
   mapM_ (\t -> inGameDo (_gameName g) $ execGameEvent $ TimeEvent t) ts


-- | update player settings
playerSettings :: PlayerSettings -> PlayerNumber -> StateT Session IO ()
playerSettings playerSettings pn = modifyProfile pn (pPlayerSettings .~ playerSettings)

playAs :: Maybe PlayerNumber -> PlayerNumber -> GameName -> StateT Session IO ()
playAs playAs pn g = inGameDo g $ do
   info pn $ unwords ["playing as ", show playAs, "in game" ++ g]
   pls <- use (game . players)
   case find ((== pn) . view playerNumber) pls of
      Nothing -> warn pn "player not in game"
      Just pi -> (game . players) .= replaceWith ((== pn) . view playerNumber) (pi {_playingAs = playAs}) pls

adminPass :: String -> PlayerNumber -> StateT Session IO ()
adminPass pass pn = do
   s <- get
   if pass == (_adminPassword $ _mSettings $ _multi s) then do
      info pn "getting admin rights"
      modifyProfile pn $ pIsAdmin .~ True
   else do
      warn pn "submitted wrong admin password"
      modifyProfile pn $ pIsAdmin .~ False

globalSettings :: Bool -> StateT Session IO ()
globalSettings mails = (multi . mSettings . mailSettings . sendMails) .= mails

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
   case find ((==gn) . view gameNameLens) gs of
      Nothing -> warn 0 "No game by that name"
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
      Nothing -> warn 0 "thread timed out, session discarded"

evalSession :: StateT Session IO () -> Session -> IO Session
evalSession sm s = do
   s' <- execStateT sm s
   writeFile nullFileName $ show $ _multi s' --dirty hack to force deep evaluation --deepseq (_multi s') (return ())
   return s'

warn, info :: (MonadIO m) => Int -> String -> m ()
info pn s = liftIO $ infoM "Nomyx.Core.Session" ("Player " ++ (show pn) ++ " " ++ s)
warn pn s = liftIO $ warningM "Nomyx.Core.Session" ("Player " ++ (show pn) ++ " " ++ s)
