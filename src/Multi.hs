{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators,
    TypeSynonymInstances, FlexibleInstances, GADTs, NamedFieldPuns, DoAndIfThenElse #-}

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
import Game
import Utils
import Interpret
import Language.Nomyx.Expression
import Data.Time
import Language.Haskell.Interpreter.Server (ServerHandle)
import Language.Nomyx.Evaluation
import Data.Maybe
import Types
import Control.Applicative
import Control.Exception
import Debug.Trace.Helpers
import Data.Lens


-- | helper function to change a player's ingame status.
mayJoinGame :: Maybe GameName -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
mayJoinGame maybename pn pl = case find (\(PlayerMulti mypn _ _ _ _ _) -> mypn == pn) pl of
                     Just o -> replace o o{ _inGame = maybename} pl
                     Nothing -> pl

newPlayerU :: PlayerMulti -> State Multi ()
newPlayerU pm = void $ mPlayers %= (pm:)


getNewPlayerNumber :: State Multi PlayerNumber
getNewPlayerNumber = do
   ps <- access mPlayers
   return $ length ps + 1


addNewGame :: Game -> State Multi ()
addNewGame new = void $ games %= (new:)

getGameByName :: GameName -> State Multi (Maybe Game)
getGameByName gn =  fmap (find (\(Game {gameName = n}) -> n==gn)) (access games)

joinGamePlayer :: PlayerNumber -> GameName -> State Multi ()
joinGamePlayer pn game = void $ mPlayers %= mayJoinGame (Just game) pn


leaveGameU :: PlayerNumber -> State Multi ()
leaveGameU pn = void $ mPlayers %= mayJoinGame Nothing pn

-- | starts a new game
newGame :: GameName -> GameDesc -> PlayerNumber -> State Multi ()
newGame name desc pn = do
   gs <- access games
   case null $ filter (\p -> gameName p == name) gs of
      True -> do
         tracePN pn $ "Creating a new game of name: " ++ name
         t <- access mCurrentTime
         -- create a game with zero players
         void $ games ~= (initialGame name desc t):gs
      False -> tracePN pn "this name is already used"

uniqueGame :: String -> [Game] -> Bool
uniqueGame s gs = null $ filter (\p -> gameName p == s) gs

-- | join a game.
joinGame :: GameName -> PlayerNumber -> State Multi ()
joinGame game pn = do
   mg <- getGameByName game
   case mg of
      Nothing -> tracePN pn "No game by that name"
      Just g -> do
         subscribeGame (gameName g) pn
         joinGamePlayer pn game


-- | leave a game (you remain subscribed).
leaveGame :: PlayerNumber -> State Multi ()
leaveGame pn = do
   leaveGameU pn
   tracePN pn "You left the game (you remain subscribed)."


-- | subcribe to a game.
subscribeGame :: GameName -> PlayerNumber -> State Multi ()
subscribeGame game pn = do
   m <- get
   inGameDo game $ do
      g <- get
      case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
         Just _ -> return ()
         Nothing -> do
            tracePN pn $ "Subscribing to game: " ++ game
            let player = PlayerInfo { playerNumber = pn, playerName = getPlayersName pn m}
            put g {players = player : (players g)}
            triggerEvent (Player Arrive) (PlayerData player)


-- | subcribe to a game.
unsubscribeGame :: GameName -> PlayerNumber -> State Multi ()
unsubscribeGame game pn = inGameDo game $ do
   g <- get
   case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
      Nothing -> tracePN pn "Not subscribed!"
      Just _ -> do
         tracePN pn $ "Unsubscribing to game: " ++ game
         let player = PlayerInfo { playerNumber = pn, playerName = getPlayersName' g pn}
         put g {players = filter (\PlayerInfo { playerNumber = mypn} -> mypn /= pn) (players g)}
         triggerEvent (Player Leave) (PlayerData player)


showSubGame :: GameName -> PlayerNumber -> State Multi ()
showSubGame g _ = inGameDo g $ do
   ps <- gets players
   traceM $ concatMap show ps

showSubscribtion :: PlayerNumber -> State Multi ()
showSubscribtion pn = inPlayersGameDo_ pn $ do
   ps <- gets players
   traceM $ concatMap show ps




-- | insert a rule in pending rules.
submitRule :: SubmitRule -> PlayerNumber -> ServerHandle -> StateT Multi IO ()
submitRule sr pn sh = do
   mnr <- enterRule sr pn sh
   tracePN pn $ "proposed " ++ (show sr)
   case mnr of
      Just nr -> do
         inPlayersGameDo' pn $ do
            r <- evProposeRule nr
            if r == True then tracePN pn $ "Your rule has been added to pending rules."
            else tracePN pn $ "Error: Rule could not be proposed"
         liftT $ updateLastRule Nothing pn
      Nothing -> liftT $ updateLastRule (Just sr) pn


-- | reads a rule.
enterRule :: SubmitRule -> PlayerNumber -> ServerHandle -> StateT Multi IO (Maybe Rule)
enterRule (SubmitRule name desc code) pn sh = do
   mrr <- lift $ interpretRule code sh
   join <$> (inPlayersGameDo' pn $ do
      rs <- gets rules
      let rn = getFreeNumber $ map rNumber rs
      case mrr of
         Right ruleFunc -> return $ Just Rule {rNumber = rn,
                      rName = name,
                      rDescription = desc,
                      rProposedBy = pn,
                      rRuleCode = code,
                      rRuleFunc = ruleFunc,
                      rStatus = Pending,
                      rAssessedBy = Nothing}
         Left e -> do
            output ("Compiler error: " ++ show e ++ "\n") pn
            tracePN pn ("Compiler error: " ++ show e ++ "\n")
            return Nothing)

updateLastRule :: Maybe SubmitRule -> PlayerNumber -> State Multi ()
updateLastRule msr pn = do
   pm <- fromJust <$> findPlayer' pn
   pls <- access mPlayers
   let pls' = replace pm (pm {_lastRule = msr}) pls
   void $ mPlayers ~= pls'

--cpuTimeLimitSoft = ResourceLimit 4
--cpuTimeLimitHard = ResourceLimit 5
--limits :: [(Resource, ResourceLimits)]
--limits = [ (ResourceCPUTime,      ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)]

inputChoiceResult :: EventNumber -> Int -> PlayerNumber -> State Multi ()
inputChoiceResult eventNumber choiceIndex pn = do
   tracePN pn $ "input choice result: Event " ++ (show eventNumber) ++ ", choice " ++  (show choiceIndex)
   inPlayersGameDo_ pn $ triggerChoice eventNumber choiceIndex

-- TODO maybe homogeneise both inputs event
inputStringResult :: Event InputString -> String -> PlayerNumber -> State Multi ()
inputStringResult event input pn = inPlayersGameDo_ pn $ triggerEvent event (InputStringData input)

inputUpload :: PlayerNumber -> FilePath -> String -> ServerHandle -> StateT Multi IO ()
inputUpload pn dir mod sh = do
    m <- lift $ loadModule dir mod sh
    tracePN pn $ " uploaded " ++ (show mod)
    case m of
      Right _ -> do
         inPlayersGameDo'_ pn $ output ("File loaded: " ++ show dir ++ " Module " ++ show mod ++"\n") pn
         tracePN pn "upload success"
         return ()
      Left e -> do
         inPlayersGameDo'_ pn $ output ("Compiler error: " ++ show e ++ "\n") pn
         tracePN pn "upload failed"
         return ()


mailSettings :: MailSettings -> PlayerNumber -> State Multi ()
mailSettings mailSettings pn = do
   mps <- access mPlayers
   case find ((==) pn . getL mPlayerNumber) mps of
      Nothing -> tracePN pn "settings not modified!"
      Just pm -> do
         tracePN pn $ "mail settings " ++ (show mailSettings)
         let newmps = replace pm pm{_mMail=mailSettings} mps
         void $ mPlayers ~= newmps

-- | show the constitution.
showConstitution :: PlayerNumber -> State Multi ()
showConstitution pn = inPlayersGameDo_ pn $ get >>= (traceM  .  show  .  activeRules)


-- | show every rules (including pendings and deleted)
showAllRules :: PlayerNumber -> State Multi ()	
showAllRules pn = inPlayersGameDo_ pn $ get >>= (traceM . show . rules)

displayPlayer :: PlayerMulti -> String
displayPlayer (PlayerMulti pn name _ _ (Just game) _) = show pn ++ ": " ++ name ++ " in game: " ++ game ++ "\n"
displayPlayer (PlayerMulti pn name _ _ Nothing _)     = show pn ++ ": " ++ name ++ "\n"


-- | Utility functions

-- | replace the player's name in the list
setName :: String -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
setName name pn pl = case find (\(PlayerMulti h _ _ _ _ _) -> h == pn) pl of
                        Just o -> replace o o{ _mPlayerName = name} pl
                        Nothing -> pl



-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> State Game a -> State Multi (Maybe a)
inPlayersGameDo pn action = do
   multi <- get
   t <- access mCurrentTime
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> tracePN pn "You must be in a game" >> return Nothing
      Just g -> do
         (a, myg) <- lift $ runStateT action (g { currentTime = t})
         modifyGame myg
         return (Just a)

inPlayersGameDo_ :: PlayerNumber -> State Game a -> State Multi ()
inPlayersGameDo_ pn action = inPlayersGameDo pn action >> return ()

inPlayersGameDo' :: PlayerNumber -> State Game a -> StateT Multi IO (Maybe a)
inPlayersGameDo' pn gs = do
    m <- get
    let (a, m') = runState (inPlayersGameDo pn gs) m
    put m'
    return a

inPlayersGameDo'_ :: PlayerNumber -> State Game a -> StateT Multi IO ()
inPlayersGameDo'_ pn action = inPlayersGameDo' pn action >> return ()

inGameDo :: GameName -> State Game () -> State Multi ()
inGameDo game action = do
   gs <- access games
   case find (\(Game {gameName =n}) -> n==game) gs of
      Nothing -> traceM "No game by that name"
      Just g -> do
         t <- access mCurrentTime
         let myg = execWithGame t action g
         modifyGame myg


triggerTimeEvent :: UTCTime -> StateT Multi IO ()
triggerTimeEvent t = do
   gs <- access games
   gs' <- lift $ mapM (\g -> trig t g `catch` timeExceptionHandler t g) gs
   void $ games ~= gs'


trig :: UTCTime -> Game -> IO Game
trig t g =  do
   let g' = execWithGame t (evTriggerTime t) g
   evaluate g'
   return g'

timeExceptionHandler :: UTCTime -> Game -> ErrorCall -> IO Game
timeExceptionHandler t g e = do
   putStrLn $ "Error in triggerTimeEvent: " ++ (show e)
   return $ execWithGame t (outputAll $ "Error while triggering a time event: " ++ (show e) ++
                           "\nThe event have been canceled. Please remove/fix the faulty rule.") g

