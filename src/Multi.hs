{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators, TemplateHaskell, QuasiQuotes,
    TypeSynonymInstances, FlexibleInstances, GADTs, NamedFieldPuns, DoAndIfThenElse, RecordWildCards #-}

-- | This module manages multi-player games and commands.
module Multi where
--Multi, PlayerPassword, GetMulti(..), FindPlayer(..), PlayerMulti(..), GetNewPlayerNumber(..), NewPlayerU(..),
--    listGame, newGame, joinGame, leaveGame, subscribeGame, unsubscribeGame, showSubscribtion, showSubGame, newPlayer,
--    submitRule, myCatch, submitRuleI, showConstitution, showAllRules, listPlayers, amendConstitution, showPendingActions,
--    showMyPendingActions, doActionsI, doAction', showCompletedActions, quit, getPendingActions, doAction, games, getPlayersGame,
--    getPlayersName) where


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
import Language.Nomyx.Game as G
import Control.Category hiding ((.))
import qualified Data.Acid.Advanced as A (update', query')
import Quotes (cr)


-- | add a new player
newPlayer :: PlayerNumber -> PlayerSettings -> Maybe GameName -> Maybe SubmitRule -> StateT Session IO ()
newPlayer uid ms gn sr = do
   s <- get
   A.update' (acidProfileData $ _profiles s) (NewProfileData uid ms gn sr)
   return ()

-- | starts a new game
newGame :: GameName -> GameDesc -> PlayerNumber -> StateT Session IO ()
newGame name desc pn = do
   sh <- access sh
   focus multi $ do
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
   name <- lift $ getPlayersName pn s
   focus multi $ inGameDo game $ G.update $ JoinGame pn name
   viewGamePlayer game pn

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
         modifyProfile pn (pLastRule ^= Nothing)
      Left e -> do
         inPlayersGameDo_ pn $ update $ Log (Just pn) ("Compiler error: " ++ show e ++ "\n")
         tracePN pn ("Compiler error: " ++ show e ++ "\n")
         modifyProfile pn (pLastRule ^= Just sr) -- keep in memory the last rule proposed by the player to display it in case of error


-- | result of choice with radio buttons
inputChoiceResult :: EventNumber -> Int -> PlayerNumber -> StateT Session IO ()
inputChoiceResult eventNumber choiceIndex pn = inPlayersGameDo_ pn $ update $ InputChoiceResult pn eventNumber choiceIndex

-- TODO maybe homogeneise both inputs event
-- | result of choice with text field
inputStringResult :: Event InputString -> String -> PlayerNumber -> StateT Session IO ()
inputStringResult (InputString _ ti) input pn = inPlayersGameDo_ pn $ update $ InputStringResult pn ti input

-- | upload a rule file
inputUpload :: PlayerNumber -> FilePath -> String -> ServerHandle -> StateT Session IO ()
inputUpload pn dir mod sh = do
   m <- liftIO $ loadModule dir mod sh
   tracePN pn $ " uploaded " ++ (show mod)
   case m of
      Right _ -> do
         inPlayersGameDo_ pn $ update $ Log (Just pn) ("File loaded: " ++ show dir ++ " Module " ++ show mod ++"\n")
         tracePN pn "upload success"
         return ()
      Left e -> do
         inPlayersGameDo_ pn $ update $ Log (Just pn) ("Compiler error: " ++ show e ++ "\n")
         tracePN pn "upload failed"
         return ()

-- | update player settings
playerSettings :: PlayerSettings -> PlayerNumber -> StateT Session IO ()
playerSettings playerSettings pn = modifyProfile pn (pPlayerSettings ^= playerSettings)


-- | Utility functions

getNewPlayerNumber :: StateT Session IO PlayerNumber
getNewPlayerNumber = do
   s <- get
   pfd <- A.query' (acidProfileData $ _profiles s) AskProfileDataNumber
   return $ pfd + 1


getGameByName :: GameName -> StateT Multi IO (Maybe LoggedGame)
getGameByName gn =  (find ((==gn) . getL (game >>> gameName))) <$> (access games)

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

inPlayersGameDo_ :: PlayerNumber -> StateT LoggedGame IO a -> StateT Session IO ()
inPlayersGameDo_ pn action = inPlayersGameDo pn action >> return ()

inGameDo :: GameName -> StateT LoggedGame IO  () -> StateT Multi IO ()
inGameDo gn action = do
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
   g' <- execWithGame' t (update $ TimeEvent t) g
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
   where addR = update' (Just $ getRuleFunc sh) . SystemAddRule

initialLoggedGame :: GameName -> GameDesc -> UTCTime -> ServerHandle -> IO LoggedGame
initialLoggedGame name desc date sh = do
   let lg = LoggedGame (emptyGame name desc date) []
   execStateT (initialGame sh) lg

displayMulti :: Multi -> String
displayMulti m = concatMap (displayGame . _game) (_games m)
