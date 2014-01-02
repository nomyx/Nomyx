{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators, TemplateHaskell, QuasiQuotes,
    TypeSynonymInstances, FlexibleInstances, GADTs, NamedFieldPuns, DoAndIfThenElse, RecordWildCards #-}

-- | This module manages multi-player games and commands.
module Multi where

import Prelude
import Data.List
import Control.Monad.State
import Utils
import Interpret
import Data.Time as T
import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Maybe
import Types
import Control.Applicative
import Control.Exception
import Debug.Trace.Helpers
import Data.Lens
import Language.Nomyx
import Language.Nomyx.Engine as G
import Control.Category hiding ((.))
import qualified Data.Acid.Advanced as A (update', query')
import Quotes (cr)


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

-- | del a game.delGame :: GameName -> StateT Session IO ()
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

-- | upload a rule file
inputUpload :: PlayerNumber -> FilePath -> String -> ServerHandle -> StateT Session IO ()
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
         tracePN pn "upload failed"
         modifyProfile pn (pLastUpload ^= UploadFailure (temp, errorMsg))

-- | update player settings
playerSettings :: PlayerSettings -> PlayerNumber -> StateT Session IO ()
playerSettings playerSettings pn = modifyProfile pn (pPlayerSettings ^= playerSettings)

playAsSetting :: (Maybe PlayerNumber) -> PlayerNumber -> StateT Session IO ()
playAsSetting mpn pn = modifyProfile pn ((pAdmin >>> pPlayAs) ^= mpn)

adminPass :: String -> PlayerNumber -> StateT Session IO ()
adminPass pass pn = do
   s <- get
   if (pass == (_adminPassword $ _mSettings $ _multi s)) then do
      tracePN pn "getting admin rights"
      modifyProfile pn $ (pAdmin >>> isAdmin) ^= True
   else do
      tracePN pn "submitted wrong admin password"
      modifyProfile pn $ (pAdmin >>> isAdmin) ^= False

globalSettings :: Bool -> StateT Session IO ()
globalSettings mails = void $ (multi >>> mSettings >>> sendMails) ~= mails

-- | Utility functions

getNewPlayerNumber :: StateT Session IO PlayerNumber
getNewPlayerNumber = do
   s <- get
   pfd <- A.query' (acidProfileData $ _profiles s) AskProfileDataNumber
   return $ pfd + 1


getGameByName :: GameName -> StateT Multi IO (Maybe LoggedGame)
getGameByName gn =  (find ((==gn) . getL (game >>> gameName))) <$> (access games)

startSimulation :: GameName -> PlayerNumber -> StateT Session IO ()
startSimulation gn pn = focus multi $ do
   gms <- access games
   case filter ((== gn) . getL (game >>> gameName)) gms of
      g:[] -> do
         tracePN pn $ "Creating a simulation for game: " ++ gn
         time <- liftIO $ T.getCurrentTime
         let sim = Simulation gn pn time
         let g' = ((game >>> gameName) ^= ("Simulated " ++ gn)) . ((game >>> simu) ^= Just sim) $ g
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


triggerTimeEvent :: UTCTime -> StateT Multi IO ()
triggerTimeEvent t = do
   gs <- access games
   gs' <- lift $ mapM (\g -> trig t g) gs
   void $ games ~= gs'


trig :: UTCTime -> LoggedGame -> IO LoggedGame
trig t g =  do
   g' <- execWithGame' t (execGameEvent $ TimeEvent t) g
   evaluate g'

-- | get all events that has not been triggered yet
getTimeEvents :: UTCTime -> Multi -> IO([UTCTime])
getTimeEvents now m = do
    let times = catMaybes $ map getTimes $ (concatMap (getL $ game >>> events)) $ _games  m
    return $ filter (\t -> t <= now && t > (-2) `addUTCTime` now) times

-- | the initial rule set for a game.
rVoteUnanimity = SubmitRule "Unanimity Vote"
                            "A proposed rule will be activated if all players vote for it"
                            [cr|onRuleProposed $ voteWith_ unanimity $ assessOnEveryVote |]

rVictory5Rules = SubmitRule "Victory 5 accepted rules"
                            "Victory is achieved if you have 5 active rules"
                            [cr|victoryXRules 5|]

rVoteMajority = SubmitRule "Majority Vote"
                            "A proposed rule will be activated if a majority of players is reached, with a minimum of 2 players, and within oone day"
                            [cr|onRuleProposed $ voteWith_ (majority `withQuorum` 2) $ assessOnEveryVote >> assessOnTimeDelay oneMinute |]


initialGame :: ServerHandle -> StateT LoggedGame IO ()
initialGame sh = mapM_ addR [rVoteUnanimity, rVictory5Rules]
   where addR r = execGameEvent' (Just $ getRuleFunc sh) (SystemAddRule r)

initialLoggedGame :: GameName -> GameDesc -> UTCTime -> ServerHandle -> IO LoggedGame
initialLoggedGame name desc date sh = do
   let lg = LoggedGame (emptyGame name desc date) []
   execStateT (initialGame sh) lg

displayMulti :: Multi -> String
displayMulti m = concatMap (displayGame . _game) (_games m)
