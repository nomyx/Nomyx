{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators,
    TypeSynonymInstances, FlexibleInstances, GADTs, NamedFieldPuns, DoAndIfThenElse, RecordWildCards #-}

-- | This module manages multi-player games and commands.
module Multi where
--Multi, PlayerPassword, GetMulti(..), FindPlayer(..), PlayerMulti(..), GetNewPlayerNumber(..), NewPlayerU(..),
--    listGame, newGame, joinGame, leaveGame, subscribeGame, unsubscribeGame, showSubscribtion, showSubGame, newPlayer,
--    submitRule, myCatch, submitRuleI, showConstitution, showAllRules, listPlayers, amendConstitution, showPendingActions,
--    showMyPendingActions, doActionsI, doAction', showCompletedActions, quit, getPendingActions, doAction, games, getPlayersGame,
--    getPlayersName) where


import Prelude hiding (catch)
import Data.List
import Control.Monad.State
--import Game
import Utils
import Interpret
-- import Language.Nomyx
import Data.Time
import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Maybe
import Types
import Control.Applicative
import Control.Exception
import Debug.Trace.Helpers
import Data.Lens
import Language.Nomyx
import Language.Nomyx.Game as G
import Control.Category hiding ((.))
import qualified Data.Acid.Advanced as A (update', query')


-- | add a new player
newPlayer :: PlayerNumber -> PlayerSettings -> Maybe GameName -> Maybe SubmitRule -> StateT Session IO ()
newPlayer uid ms gn sr = do
   s <- get
   A.update' (acidProfileData $ _acid s) (NewProfileData uid ms gn sr)
   return ()

-- | starts a new game
newGame :: GameName -> GameDesc -> PlayerNumber -> StateT Session IO ()
newGame name desc pn = focus multi $ do
   gs <- access games
   case null $ filter ((== name) . getL (game >>> gameName)) gs of
      True -> do
         tracePN pn $ "Creating a new game of name: " ++ name
         t <- access mCurrentTime
         -- create a game with zero players
         void $ games %= ((initialLoggedGame name desc t) : )
      False -> tracePN pn "this name is already used"

-- | view a game.
viewGamePlayer :: GameName -> PlayerNumber -> StateT Session IO ()
viewGamePlayer game pn = do
   mg <- focus multi $ getGameByName game
   case mg of
      Nothing -> tracePN pn "No game by that name"
      Just _ -> do
         s <- get
         pfd <- A.query' (acidProfileData $ _acid s) (AskProfileData pn)
         A.update' (acidProfileData $ _acid s) (SetProfileData (fromJust pfd){_pViewingGame = Just game})
         return ()

-- | unview a game.
unviewGamePlayer :: PlayerNumber -> StateT Session IO ()
unviewGamePlayer pn = do
   s <- get
   pfd <- A.query' (acidProfileData $ _acid s) (AskProfileData pn)
   A.update' (acidProfileData $ _acid s) (SetProfileData (fromJust pfd){_pViewingGame = Nothing})
   return ()

-- | join a game.
joinGame :: GameName -> PlayerNumber -> StateT Session IO ()
joinGame game pn = do
      s <- get
      name <- lift $ getPlayersName pn s
      focus multi $ inGameDo game $ G.update $ JoinGame pn name

-- | leave a game.
leaveGame :: GameName -> PlayerNumber -> StateT Session IO ()
leaveGame game pn = focus multi $ inGameDo game $ G.update $ LeaveGame pn


-- | insert a rule in pending rules.
submitRule :: SubmitRule -> PlayerNumber -> ServerHandle -> StateT Session IO ()
submitRule sr@(SubmitRule _ _ code) pn sh = do
   tracePN pn $ "proposed " ++ (show sr)
   mrr <- lift $ interpretRule code sh
   case mrr of
      Right _ -> do
         tracePN pn $ "proposed rule compiled OK "
         inPlayersGameDo_ pn $ G.update' (Just $ getRuleFunc sh) (ProposeRuleEv pn sr)
         updateLastRule Nothing pn
      Left e -> do
         inPlayersGameDo_ pn $ update $ OutputPlayer pn ("Compiler error: " ++ show e ++ "\n")
         tracePN pn ("Compiler error: " ++ show e ++ "\n")
         updateLastRule (Just sr) pn

-- | keep in memory the last rule proposed by the player to display it in case of error
updateLastRule :: Maybe SubmitRule -> PlayerNumber -> StateT Session IO ()
updateLastRule msr pn = do
   s <- get
   pfd <- A.query' (acidProfileData $ _acid s) (AskProfileData pn)
   A.update' (acidProfileData $ _acid s) (SetProfileData (fromJust pfd){_pLastRule = msr})
   return ()

-- | result of choice with radio buttons
inputChoiceResult :: EventNumber -> Int -> PlayerNumber -> StateT Session IO ()
inputChoiceResult eventNumber choiceIndex pn = do
   tracePN pn $ "input choice result: Event " ++ (show eventNumber) ++ ", choice " ++  (show choiceIndex)
   inPlayersGameDo_ pn $ update $ InputChoiceResult pn eventNumber choiceIndex

-- TODO maybe homogeneise both inputs event
-- | result of choice with text field
inputStringResult :: Event InputString -> String -> PlayerNumber -> StateT Session IO ()
inputStringResult (InputString _ ti) input pn = inPlayersGameDo_ pn $ update $ InputStringResult pn ti input

-- | upload a rule file
inputUpload :: PlayerNumber -> FilePath -> String -> ServerHandle -> StateT Session IO ()
inputUpload pn dir mod sh = do
   --sh <- access (mSettings >>> sh)
   m <- liftIO $ loadModule dir mod sh
   tracePN pn $ " uploaded " ++ (show mod)
   case m of
      Right _ -> do
         inPlayersGameDo_ pn $ update $ OutputPlayer pn ("File loaded: " ++ show dir ++ " Module " ++ show mod ++"\n")
         tracePN pn "upload success"
         return ()
      Left e -> do
         inPlayersGameDo_ pn $ update $ OutputPlayer pn ("Compiler error: " ++ show e ++ "\n")
         tracePN pn "upload failed"
         return ()

-- | update player settings
playerSettings :: PlayerSettings -> PlayerNumber -> StateT Session IO ()
playerSettings playerSettings pn = do
   s <- get
   pfd <- A.query' (acidProfileData $ _acid s) (AskProfileData pn)
   case pfd of
      Nothing -> tracePN pn "settings not modified!"
      Just pf -> do
         tracePN pn $ "player settings " ++ (show playerSettings)
         A.update' (acidProfileData $ _acid s) (SetProfileData pf{_pPlayerSettings=playerSettings})
         return ()


-- | Utility functions

getNewPlayerNumber :: StateT Session IO PlayerNumber
getNewPlayerNumber = do
   s <- get
   pfd <- A.query' (acidProfileData $ _acid s) AskProfileDataNumber
   return $ pfd + 1


getGameByName :: GameName -> StateT Multi IO (Maybe LoggedGame)
getGameByName gn =  (find ((==gn) . getL (game >>> gameName))) <$> (access games)

-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> StateT LoggedGame IO a -> StateT Session IO (Maybe a)
inPlayersGameDo pn action = do
   s <- get
   t <- access (multi >>> mCurrentTime) --TODO remove?
   mg <- lift $ getPlayersGame pn s
   case mg of
      Nothing -> tracePN pn "You must be in a game" >> return Nothing
      Just g -> do
         (a, myg) <- lift $ runStateT action (setL (game >>> currentTime) t g)
         focus multi $ modifyGame myg
         return (Just a)

inPlayersGameDo_ :: PlayerNumber -> StateT LoggedGame IO a -> StateT Session IO ()
inPlayersGameDo_ pn action = inPlayersGameDo pn action >> return ()

inGameDo :: GameName -> StateT LoggedGame IO  () -> StateT Multi IO ()
inGameDo gn action = do
   (gs :: [LoggedGame]) <- access games
   case find ((==gn) . getL (game >>> gameName)) gs of
      Nothing -> traceM "No game by that name"
      Just (g::LoggedGame) -> do
         t <- access mCurrentTime
         myg <- lift $ execWithGame' t action g
         modifyGame myg

--TODO push this down to Game?
triggerTimeEvent :: UTCTime -> StateT Multi IO ()
triggerTimeEvent t = do
   gs <- access games
   gs' <- lift $ mapM (\g -> trig t g `catch` timeExceptionHandler t g) gs
   void $ games ~= gs'


trig :: UTCTime -> LoggedGame -> IO LoggedGame
trig t g =  do
   g' <- execWithGame' t (update $ TimeEvent t) g
   evaluate g'


timeExceptionHandler :: UTCTime -> LoggedGame -> ErrorCall -> IO LoggedGame
timeExceptionHandler t g e = do
   putStrLn $ "Error in triggerTimeEvent: " ++ (show e)
   execWithGame' t (G.outputAll $ "Error while triggering a time event: " ++ (show e) ++
                   "\nThe event have been canceled. Please remove/fix the faulty rule.") g

-- | get all events that has not been triggered yet
getTimeEvents :: UTCTime -> Multi -> IO([UTCTime])
getTimeEvents now m = do
    let times = catMaybes $ map getTimes $ (concatMap (getL $ game >>> events)) $ _games  m
    return $ filter (\t -> t <= now && t > (-2) `addUTCTime` now) times

-- | the initial rule set for a game.
rVoteUnanimity = Rule  {
    _rNumber       = 1,
    _rName         = "Unanimity Vote",
    _rDescription  = "A proposed rule will be activated if all players vote for it",
    _rProposedBy   = 0,
    _rRuleCode     = "onRuleProposed $ voteWith unanimity",
    _rRuleFunc     = onRuleProposed $ voteWith unanimity $ assessOnEveryVotes,
    _rStatus       = Active,
    _rAssessedBy   = Nothing}

rVictory5Rules = Rule  {
    _rNumber       = 2,
    _rName         = "Victory 5 accepted rules",
    _rDescription  = "Victory is achieved if you have 5 active rules",
    _rProposedBy   = 0,
    _rRuleCode     = "victoryXRules 5",
    _rRuleFunc     = victoryXRules 5,
    _rStatus       = Active,
    _rAssessedBy   = Nothing}

initialGame :: GameName -> GameDesc -> UTCTime -> Game
initialGame name desc date = flip execState (emptyGame name desc date) $ do
    evAddRule rVoteUnanimity
    evActivateRule (_rNumber rVoteUnanimity) 0
    evAddRule rVictory5Rules
    evActivateRule (_rNumber rVictory5Rules) 0

initialLoggedGame :: GameName -> GameDesc -> UTCTime -> LoggedGame
initialLoggedGame name desc date = (LoggedGame (initialGame name desc date) [])

