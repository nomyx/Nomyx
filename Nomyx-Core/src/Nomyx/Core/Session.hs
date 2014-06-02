{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | This module manages multi-player commands.
module Nomyx.Core.Session where

import Language.Haskell.Interpreter.Server (ServerHandle)
import Language.Haskell.Interpreter (InterpreterError)
import Debug.Trace.Helpers
import Data.Lens
import Data.Time as T
import Data.List
import qualified Data.Acid.Advanced as A (update', query')
import Control.Category hiding ((.))
import Control.Monad.State
import Control.Concurrent.STM
import System.IO.PlafCompat
import Language.Nomyx
import Nomyx.Core.Types
import Nomyx.Core.Utils
import Nomyx.Core.Multi
import Nomyx.Core.Profile
import Nomyx.Core.Interpret
import Nomyx.Core.Serialize
import Nomyx.Core.Engine as G

-- | add a new player
newPlayer :: PlayerNumber -> PlayerSettings -> StateT Session IO ()
newPlayer uid ms = do
   s <- get
   void $ A.update' (acidProfileData $ _profiles s) (NewProfileData uid ms)

-- | starts a new game
newGame :: GameName -> GameDesc -> PlayerNumber -> Bool -> StateT Session IO ()
newGame name desc pn isPublic = do
   sh <- access sh
   focus multi $ newGame' name desc pn isPublic sh

newGame' :: GameName -> GameDesc -> PlayerNumber -> Bool -> ServerHandle -> StateT Multi IO ()
newGame' name desc pn isPublic sh = do
      gs <- access gameInfos
      if not $ any ((== name) . getL gameNameLens) gs then do
         tracePN pn $ "Creating a new game of name: " ++ name
         t <- lift T.getCurrentTime
         -- create a game with zero players
         lg <- lift $ initialGameInfo name desc isPublic (Just pn) t sh
         void $ gameInfos %= (lg : )
      else tracePN pn "this name is already used"

-- | view a game.
viewGamePlayer :: GameName -> PlayerNumber -> StateT Session IO ()
viewGamePlayer gn pn = do
   mg <- focus multi $ getGameByName gn
   case mg of
      Nothing -> tracePN pn "No game by that name"
      Just _ -> modifyProfile pn (pViewingGame ^= Just gn)

-- | unview a game.
unviewGamePlayer :: PlayerNumber -> StateT Session IO ()
unviewGamePlayer pn = modifyProfile pn (pViewingGame ^= Nothing)

-- | join a game (also view it for conveniency)
joinGame :: GameName -> PlayerNumber -> StateT Session IO ()
joinGame gn pn = do
   s <- get
   name <- lift $ Nomyx.Core.Profile.getPlayerName pn s
   inGameDo gn $ G.execGameEvent $ JoinGame pn name
   viewGamePlayer gn pn

-- | delete a game.
delGame :: GameName -> StateT Session IO ()
delGame name = focus multi $ void $ gameInfos %= filter ((/= name) . getL gameNameLens)


-- | leave a game.
leaveGame :: GameName -> PlayerNumber -> StateT Session IO ()
leaveGame game pn = inGameDo game $ G.execGameEvent $ LeaveGame pn

-- | insert a rule in pending rules.
submitRule :: SubmitRule -> PlayerNumber -> GameName -> ServerHandle -> StateT Session IO ()
submitRule sr@(SubmitRule _ _ code) pn gn sh = do
   tracePN pn $ "proposed " ++ show sr
   mrr <- liftIO $ interpretRule code sh
   case mrr of
      Right _ -> do
         tracePN pn "proposed rule compiled OK "
         inGameDo gn $ G.execGameEvent' (Just $ getRuleFunc sh) (ProposeRuleEv pn sr)
         modifyProfile pn (pLastRule ^= Just (sr, "Rule submitted OK!"))
      Left e -> submitRuleError sr pn gn e

adminSubmitRule :: SubmitRule -> PlayerNumber -> GameName -> ServerHandle -> StateT Session IO ()
adminSubmitRule sr@(SubmitRule _ _ code) pn gn sh = do
   tracePN pn $ "admin proposed " ++ show sr
   mrr <- liftIO $ interpretRule code sh
   case mrr of
      Right _ -> do
         tracePN pn "proposed rule compiled OK "
         inGameDo gn $ execGameEvent' (Just $ getRuleFunc sh) (SystemAddRule sr)
         modifyProfile pn (pLastRule ^= Just (sr, "Admin rule submitted OK!"))
      Left e -> submitRuleError sr pn gn e

submitRuleError :: SubmitRule -> PlayerNumber -> GameName -> InterpreterError -> StateT Session IO ()
submitRuleError sr pn gn e = do
   let errorMsg = showInterpreterError e
   inGameDo gn $ execGameEvent $ GLog (Just pn) ("Error in submitted rule: " ++ errorMsg)
   tracePN pn ("Error in submitted rule: " ++ errorMsg)
   modifyProfile pn (pLastRule ^= Just (sr, errorMsg))

checkRule :: SubmitRule -> PlayerNumber -> ServerHandle -> StateT Session IO ()
checkRule sr@(SubmitRule _ _ code) pn sh = do
   tracePN pn $ "check rule " ++ show sr
   mrr <- liftIO $ interpretRule code sh
   case mrr of
      Right _ -> do
         tracePN pn "proposed rule compiled OK"
         modifyProfile pn (pLastRule ^= Just (sr, "Rule compiled OK. Now you can submit it!"))
      Left e -> do
         let errorMsg = showInterpreterError e
         tracePN pn ("Error in submitted rule: " ++ errorMsg)
         modifyProfile pn (pLastRule ^= Just (sr, errorMsg))

inputResult :: PlayerNumber -> EventNumber -> InputNumber -> InputData -> GameName -> StateT Session IO ()
inputResult pn en inn ir gn = inGameDo gn $ execGameEvent $ InputResult pn en inn ir

-- | upload a rule file, given a player number, the full path of the file, the file name and the server handle
inputUpload :: PlayerNumber -> FilePath -> FilePath -> ServerHandle -> StateT Session IO Bool
inputUpload pn temp mod sh = do
   saveDir <- access (multi >>> mSettings >>> saveDir)
   m <- liftIO $ loadModule temp mod sh saveDir
   tracePN pn $ " uploaded " ++ show mod
   case m of
      Nothing -> do
         inPlayersGameDo pn $ execGameEvent $ GLog (Just pn) ("File loaded: " ++ show temp ++ ", as " ++ show mod ++"\n")
         tracePN pn "upload success"
         modifyProfile pn (pLastUpload ^= UploadSuccess)
         return True
      Just e -> do
         let errorMsg = showInterpreterError e
         inPlayersGameDo pn $ execGameEvent $ GLog (Just pn) ("Error in file: " ++ show e ++ "\n")
         tracePN pn $ "upload failed: \n" ++ show e
         modifyProfile pn (pLastUpload ^= UploadFailure (temp, errorMsg))
         return False

-- | update player settings
playerSettings :: PlayerSettings -> PlayerNumber -> StateT Session IO ()
playerSettings playerSettings pn = modifyProfile pn (pPlayerSettings ^= playerSettings)

playAs :: Maybe PlayerNumber -> PlayerNumber -> GameName -> StateT Session IO ()
playAs playAs pn g = inGameDo g $ do
   pls <- access (game >>> players)
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> tracePN pn "player not in game"
      Just pi -> void $ (game >>> players) ~= replaceWith ((== pn) . getL playerNumber) (pi {_playAs = playAs}) pls

adminPass :: String -> PlayerNumber -> StateT Session IO ()
adminPass pass pn = do
   s <- get
   if pass == (_adminPassword $ _mSettings $ _multi s) then do
      tracePN pn "getting admin rights"
      modifyProfile pn $ pIsAdmin ^= True
   else do
      tracePN pn "submitted wrong admin password"
      modifyProfile pn $ pIsAdmin ^= False

globalSettings :: Bool -> StateT Session IO ()
globalSettings mails = void $ (multi >>> mSettings >>> sendMails) ~= mails

-- | Utility functions

getNewPlayerNumber :: StateT Session IO PlayerNumber
getNewPlayerNumber = do
   s <- get
   pfd <- A.query' (acidProfileData $ _profiles s) AskProfileDataNumber
   return $ pfd + 1

forkGame :: GameName -> PlayerNumber -> StateT Session IO ()
forkGame gn pn = focus multi $ do
   gms <- access gameInfos
   case filter ((== gn) . getL gameNameLens) gms of
      gi:[] -> do
         tracePN pn $ "Forking game: " ++ gn
         time <- liftIO T.getCurrentTime
         let gi' = GameInfo {
            _loggedGame     = (game >>> gameName) `setL` ("Forked " ++ gn) $ _loggedGame gi,
            _ownedBy        = Just pn,
            _forkedFromGame = Just gn,
            _isPublic       = False,
            _startedAt      = time}
         void $ gameInfos %= (gi' : )
      _ -> tracePN pn $ "Creating a simulation game: no game by that name: " ++ gn


-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> StateT LoggedGame IO a -> StateT Session IO (Maybe a)
inPlayersGameDo pn action = do
   s <- get
   t <- lift T.getCurrentTime
   mg <- lift $ getPlayersGame pn s
   case mg of
      Nothing -> tracePN pn "You must be in a game" >> return Nothing
      Just gi -> do
         (a, mylg) <- lift $ runStateT action (setL (game >>> currentTime) t (_loggedGame gi))
         focus multi $ modifyGame (gi {_loggedGame = mylg})
         return (Just a)

inGameDo :: GameName -> StateT LoggedGame IO  () -> StateT Session IO ()
inGameDo gn action = focus multi $ do
   (gs :: [GameInfo]) <- access gameInfos
   case find ((==gn) . getL gameNameLens) gs of
      Nothing -> traceM "No game by that name"
      Just (gi::GameInfo) -> do
         t <- lift $ T.getCurrentTime
         mylg <- lift $ execWithGame' t action (_loggedGame gi)
         modifyGame (gi {_loggedGame = mylg})

-- update a session by executing a command.
-- we set a watchdog in case the evaluation would not finish
updateSession :: TVar Session -> StateT Session IO () -> IO ()
updateSession ts sm = do
   s <- atomically $ readTVar ts
   ms <- evalWithWatchdog s (evalSession sm)
   case ms of
      Just s -> do
         atomically $ writeTVar ts s
         save $ _multi s
      Nothing -> putStrLn "thread timed out, updateSession discarded"


evalSession :: StateT Session IO () -> Session -> IO Session
evalSession sm s = do
   s' <- execStateT sm s
   writeFile nullFileName $ show $ _multi s' --dirty hack to force deep evaluation --deepseq (_multi s') (return ())
   return s'

