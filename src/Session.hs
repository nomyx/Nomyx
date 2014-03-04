{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | This module manages multi-player commands.
module Session where

import Prelude
import Data.List
import Control.Monad.State
import Utils
import Interpret
import Multi
import Data.Time as T
import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Maybe
import Types
import Control.Applicative
import Debug.Trace.Helpers
import Data.Lens
import Language.Nomyx
import Language.Nomyx.Engine as G
import Control.Category hiding ((.))
import qualified Data.Acid.Advanced as A (update', query')
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
--import Mueval.Parallel
import System.IO.PlafCompat
import System.Posix.Signals
import Control.Exception

-- | add a new player
newPlayer :: PlayerNumber -> PlayerSettings -> StateT Session IO ()
newPlayer uid ms = do
   s <- get
   void $ A.update' (acidProfileData $ _profiles s) (NewProfileData uid ms)

-- | starts a new game
newGame :: GameName -> GameDesc -> PlayerNumber -> StateT Session IO ()
newGame name desc pn = do
   sh <- access sh
   focus multi $ newGame' name desc pn sh

newGame' :: GameName -> GameDesc -> PlayerNumber -> ServerHandle -> StateT Multi IO ()
newGame' name desc pn sh = do
      gs <- access games
      case null $ filter ((== name) . getL (game >>> gameName)) gs of
         True -> do
            tracePN pn $ "Creating a new game of name: " ++ name
            t <- lift $ T.getCurrentTime
            -- create a game with zero players
            lg <- lift $ initialLoggedGame name desc t sh
            void $ games %= (lg : )
         False -> tracePN pn "this name is already used"

-- | view a game.
viewGamePlayer :: GameName -> PlayerNumber -> StateT Session IO ()
viewGamePlayer game pn = do
   mg <- focus multi $ getGameByName game
   case mg of
      Nothing -> tracePN pn "No game by that name"
      Just _ -> modifyProfile pn (pViewingGame ^= Just game)

-- | unview a game.
unviewGamePlayer :: PlayerNumber -> StateT Session IO ()
unviewGamePlayer pn = modifyProfile pn (pViewingGame ^= Nothing)

-- | join a game (also view it for conveniency)
joinGame :: GameName -> PlayerNumber -> StateT Session IO ()
joinGame game pn = do
   s <- get
   name <- lift $ Utils.getPlayerName pn s
   inGameDo game $ G.execGameEvent $ JoinGame pn name
   viewGamePlayer game pn

-- | del a game.
delGame :: GameName -> StateT Session IO ()
delGame name = focus multi $ void $ games %= filter ((/= name) . getL (game >>> gameName))


-- | leave a game.
leaveGame :: GameName -> PlayerNumber -> StateT Session IO ()
leaveGame game pn = inGameDo game $ G.execGameEvent $ LeaveGame pn

-- | insert a rule in pending rules.
submitRule :: SubmitRule -> PlayerNumber -> GameName -> ServerHandle -> StateT Session IO ()
submitRule sr@(SubmitRule _ _ code) pn gn sh = do
   tracePN pn $ "proposed " ++ (show sr)
   mrr <- liftIO $ interpretRule code sh
   case mrr of
      Right _ -> do
         tracePN pn $ "proposed rule compiled OK "
         inGameDo gn $ G.execGameEvent' (Just $ getRuleFunc sh) (ProposeRuleEv pn sr)
         modifyProfile pn (pLastRule ^= Just (sr, "Rule submitted OK!"))
      Left e -> do
         let errorMsg = showInterpreterError e
         inGameDo gn $ execGameEvent $ GLog (Just pn) ("Error in submitted rule: " ++ errorMsg)
         tracePN pn ("Error in submitted rule: " ++ errorMsg)
         modifyProfile pn (pLastRule ^= Just (sr, errorMsg)) -- keep in memory the last rule proposed by the player to display it in case of error

adminSubmitRule :: SubmitRule -> PlayerNumber -> GameName -> ServerHandle -> StateT Session IO ()
adminSubmitRule sr@(SubmitRule _ _ code) pn gn sh = do
   tracePN pn $ "admin proposed " ++ (show sr)
   mrr <- liftIO $ interpretRule code sh
   case mrr of
      Right _ -> do
         tracePN pn $ "proposed rule compiled OK "
         inGameDo gn $ execGameEvent' (Just $ getRuleFunc sh) (SystemAddRule sr)
         modifyProfile pn (pLastRule ^= Just (sr, "Admin rule submitted OK!"))
      Left e -> do
         let errorMsg = showInterpreterError e
         inGameDo gn $ execGameEvent $ GLog (Just pn) ("Error in submitted rule: " ++ errorMsg)
         tracePN pn ("Error in submitted rule: " ++ errorMsg)
         modifyProfile pn (pLastRule ^= Just (sr, errorMsg))

checkRule :: SubmitRule -> PlayerNumber -> ServerHandle -> StateT Session IO ()
checkRule sr@(SubmitRule _ _ code) pn sh = do
   tracePN pn $ "check rule " ++ (show sr)
   mrr <- liftIO $ interpretRule code sh
   case mrr of
      Right _ -> do
         tracePN pn $ "proposed rule compiled OK "
         modifyProfile pn (pLastRule ^= Just (sr, "Rule compiled OK. Now you can submit it!"))
      Left e -> do
         let errorMsg = showInterpreterError e
         tracePN pn ("Error in submitted rule: " ++ errorMsg)
         modifyProfile pn (pLastRule ^= Just (sr, errorMsg))

inputResult :: PlayerNumber -> EventNumber -> UInputData -> GameName -> StateT Session IO ()
inputResult pn en ir gn = inGameDo gn $ execGameEvent $ InputResult pn en ir

-- | upload a rule file, given a player number, the full path of the file, the file name and the server handle
inputUpload :: PlayerNumber -> FilePath -> FilePath -> ServerHandle -> StateT Session IO ()
inputUpload pn temp mod sh = do
   saveDir <- access (multi >>> mSettings >>> saveDir)
   m <- liftIO $ loadModule temp mod sh saveDir
   tracePN pn $ " uploaded " ++ (show mod)
   case m of
      Right _ -> do
         inPlayersGameDo pn $ execGameEvent $ GLog (Just pn) ("File loaded: " ++ show temp ++ ", as " ++ show mod ++"\n")
         tracePN pn "upload success"
         modifyProfile pn (pLastUpload ^= UploadSuccess)
      Left e -> do
         let errorMsg = showInterpreterError e
         inPlayersGameDo pn $ execGameEvent $ GLog (Just pn) ("Error in file: " ++ show e ++ "\n")
         tracePN pn $ "upload failed: \n" ++ (show e)
         modifyProfile pn (pLastUpload ^= UploadFailure (temp, errorMsg))

-- | update player settings
playerSettings :: PlayerSettings -> PlayerNumber -> StateT Session IO ()
playerSettings playerSettings pn = modifyProfile pn (pPlayerSettings ^= playerSettings)

playAs :: (Maybe PlayerNumber) -> PlayerNumber -> GameName -> StateT Session IO ()
playAs playAs pn g = inGameDo g $ do
   pls <- access (game >>> players)
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> tracePN pn "player not in game"
      Just pi -> void $ (game >>> players) ~= replaceWith ((== pn) . getL playerNumber) (pi {_playAs = playAs}) pls

adminPass :: String -> PlayerNumber -> StateT Session IO ()
adminPass pass pn = do
   s <- get
   if (pass == (_adminPassword $ _mSettings $ _multi s)) then do
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

startSimulation :: GameName -> PlayerNumber -> StateT Session IO ()
startSimulation gn pn = focus multi $ do
   gms <- access games
   case filter ((== gn) . getL (game >>> gameName)) gms of
      g:[] -> do
         tracePN pn $ "Creating a simulation for game: " ++ gn
         time <- liftIO $ T.getCurrentTime
         let sim = Simulation gn pn time
         let g' = ((game >>> gameName) ^= ("Fork of " ++ gn)) . ((game >>> simu) ^= Just sim) $ g
         void $ games %= (g' : )
      _ -> tracePN pn $ "Creating a simulation game: no game by that name: " ++ gn


-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> StateT LoggedGame IO a -> StateT Session IO (Maybe a)
inPlayersGameDo pn action = do
   s <- get
   t <- lift $ T.getCurrentTime
   mg <- lift $ getPlayersGame pn s
   case mg of
      Nothing -> tracePN pn "You must be in a game" >> return Nothing
      Just g -> do
         (a, myg) <- lift $ runStateT action (setL (game >>> currentTime) t g)
         focus multi $ modifyGame myg
         return (Just a)

inGameDo :: GameName -> StateT LoggedGame IO  () -> StateT Session IO ()
inGameDo gn action = focus multi $ do
   (gs :: [LoggedGame]) <- access games
   case find ((==gn) . getL (game >>> gameName)) gs of
      Nothing -> traceM "No game by that name"
      Just (g::LoggedGame) -> do
         t <- lift $ T.getCurrentTime
         myg <- lift $ execWithGame' t action g
         modifyGame myg

-- update a session by executing a command.
-- we set a watchdog in case the evaluation would not finish
updateSession :: (TVar Session) -> StateT Session IO () -> IO ()
updateSession ts sm = do
   s <- atomically $ readTVar ts
   ms <- updateSession' s sm
   when (isJust ms) $ atomically $ writeTVar ts $ fromJust ms

--Sets a watchdog to kill the evaluation thread if it doesn't finishes.
-- The function starts both the evaluation thread and the watchdog thread, and blocks awaiting the result.
-- Option 1: the evaluation thread finishes before the watchdog. The MVar is filled with the result,
--  which unblocks the main thread. The watchdog then finishes latter, and fills the MVar with Nothing.
-- Option 2: the watchdog finishes before the evaluation thread. The eval thread is killed, and the
--  MVar is filled with Nothing, which unblocks the main thread. The watchdog finishes.
updateSession' :: Session -> StateT Session IO () -> IO (Maybe Session)
updateSession' s sm = do
   mvar <- newEmptyMVar
   hSetBuffering stdout NoBuffering
   --start evaluation thread
   id <- forkIO $ do
      s' <- evalSession s sm
      putMVar mvar (Just s')
   --start watchdog thread
   watchDog 3 id mvar
   takeMVar mvar

-- | Fork off a thread which will sleep and then kill off the specified thread.
watchDog :: Int -> ThreadId -> MVar (Maybe Session) -> IO ()
watchDog tout tid mvar = void $ forkIO $ do
   threadDelay (tout * 1000000)
   killThread tid
   putMVar mvar Nothing

evalSession :: Session -> StateT Session IO () -> IO Session
evalSession s sm = do
   s' <- execStateT sm s
   writeFile nullFileName $ displayMulti $ _multi s' --dirty hack to force deep evaluation --deepseq (_multi s') (return ())
   return s'

