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
import Debug.Trace.Helpers()
import Language.Nomyx.Expression
import Data.Time
import Language.Haskell.Interpreter.Server (ServerHandle)
import Language.Nomyx.Evaluation
import Data.Maybe
import Types
import Control.Applicative
import Control.Exception
import Debug.Trace.Helpers

-- | helper function to change a player's ingame status.
mayJoinGame :: Maybe GameName -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
mayJoinGame maybename pn pl = case find (\(PlayerMulti mypn _ _ _ _ _) -> mypn == pn) pl of
                     Just o -> replace o o{ inGame = maybename} pl
                     Nothing -> pl

newPlayerU :: PlayerMulti -> State Multi ()
newPlayerU pm = do
   pms <- gets mPlayers
   modify (\multi -> multi { mPlayers = pm : pms})


getNewPlayerNumber :: State Multi PlayerNumber
getNewPlayerNumber = do
   ps <- gets mPlayers
   return $ length ps + 1


addNewGame :: Game -> State Multi ()
addNewGame new = modify (\multi@Multi {games=gs} -> multi {games =  new:gs})

getGameByName :: GameName -> State Multi (Maybe Game)
getGameByName gn =  fmap (find (\(Game {gameName = n}) -> n==gn)) (gets games)

joinGamePlayer :: PlayerNumber -> GameName -> State Multi ()
joinGamePlayer pn game = modify (\multi -> multi {mPlayers = mayJoinGame (Just game) pn (mPlayers multi)})


leaveGameU :: PlayerNumber -> State Multi ()
leaveGameU pn = modify (\multi -> multi {mPlayers = mayJoinGame Nothing pn (mPlayers multi)})

-- | list the active games
listGame :: PlayerNumber -> State Multi ()
listGame _ = do
   gs <- gets games
   case length gs of
      0 -> traceM "No active games"
      _ -> do
         traceM "Active games:"
         traceM $ concatMap (\g -> gameName g ++ "\n") gs

-- | starts a new game
newGame :: GameName -> String -> PlayerNumber -> State Multi ()
newGame name desc _ = do
   gs <- gets games
   case null $ filter (\p -> gameName p == name) gs of
      True -> do
         traceM $ "Creating a new game of name: " ++ name
         t <- gets mCurrentTime
         -- create a game with zero players
         modify (\m -> m {games = (initialGame name desc t):gs})
      False -> traceM $ "this name is already used"

uniqueGame :: String -> [Game] -> Bool
uniqueGame s gs = null $ filter (\p -> gameName p == s) gs

-- | join a game.
joinGame :: GameName -> PlayerNumber -> State Multi ()
joinGame game pn = do
   mg <- getGameByName game
   case mg of
      Nothing -> traceM $ "No game by that name"
      Just g -> do
         traceM "subscribing first."
         subscribeGame (gameName g) pn
         traceM $ "Joining game: " ++ game
         joinGamePlayer pn game


-- | leave a game (you remain subscribed).
leaveGame :: PlayerNumber -> State Multi ()
leaveGame pn = do
   leaveGameU pn
   traceM "You left the game (you remain subscribed)."


-- | subcribe to a game.
subscribeGame :: GameName -> PlayerNumber -> State Multi ()
subscribeGame game pn = do
   m <- get
   inGameDo game $ do
      g <- get
      case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
         Just _ -> traceM "Already subscribed!"
         Nothing -> do
            traceM $ "Subscribing to game: " ++ game
            let player = PlayerInfo { playerNumber = pn, playerName = getPlayersName pn m}
            put g {players = player : (players g)}
            triggerEvent (Player Arrive) (PlayerData player)


-- | subcribe to a game.
unsubscribeGame :: GameName -> PlayerNumber -> State Multi ()
unsubscribeGame game pn = inGameDo game $ do
   g <- get
   case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
      Nothing -> traceM "Not subscribed!"
      Just _ -> do
         traceM $ "Unsubscribing to game: " ++ game
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
   case mnr of
      Just nr -> do
         inPlayersGameDo' pn $ do
            r <- evProposeRule nr
            if r == True then traceM $ "Your rule has been added to pending rules."
            else traceM $ "Error: Rule could not be proposed"
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
            return Nothing)

updateLastRule :: Maybe SubmitRule -> PlayerNumber -> State Multi ()
updateLastRule msr pn = do
   pm <- fromJust <$> findPlayer' pn
   pls <- gets mPlayers
   let pls' = replace pm (pm {lastRule = msr}) pls
   modify (\m -> m{mPlayers = pls'})

--cpuTimeLimitSoft = ResourceLimit 4
--cpuTimeLimitHard = ResourceLimit 5
--limits :: [(Resource, ResourceLimits)]
--limits = [ (ResourceCPUTime,      ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)]

inputChoiceResult :: EventNumber -> Int -> PlayerNumber -> State Multi ()
inputChoiceResult eventNumber choiceIndex pn = inPlayersGameDo_ pn $ triggerChoice eventNumber choiceIndex

-- TODO maybe homogeneise both inputs event
inputStringResult :: Event InputString -> String -> PlayerNumber -> State Multi ()
inputStringResult event input pn = inPlayersGameDo_ pn $ triggerEvent event (InputStringData input)

inputUpload :: PlayerNumber -> FilePath -> String -> ServerHandle -> StateT Multi IO ()
inputUpload pn dir mod sh = do
    m <- lift $ loadModule dir mod sh
    case m of
      Right _ -> do
         inPlayersGameDo'_ pn $ output ("File loaded: " ++ show dir ++ " Module " ++ show mod ++"\n") pn
         return ()
      Left e -> do
         inPlayersGameDo'_ pn $ output ("Compiler error: " ++ show e ++ "\n") pn
         return ()


mailSettings :: MailSettings -> PlayerNumber -> State Multi ()
mailSettings mailSettings pn = do
   mps <- gets mPlayers
   case find (\(PlayerMulti {mPlayerNumber}) -> pn==mPlayerNumber) mps of
      Nothing -> traceM "settings not modified!"
      Just pm -> do
         let newmps = replace pm pm{mMail=mailSettings} mps
         modify (\m -> m{mPlayers = newmps})

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
                        Just o -> replace o o{ mPlayerName = name} pl
                        Nothing -> pl



-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> State Game a -> State Multi (Maybe a)
inPlayersGameDo pn action = do
   multi <- get
   t <- gets mCurrentTime
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> traceM "You must be in a game" >> return Nothing
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
   gs <- gets games
   case find (\(Game {gameName =n}) -> n==game) gs of
      Nothing -> traceM "No game by that name"
      Just g -> do
         t <- gets mCurrentTime
         let myg = execWithGame t action g
         modifyGame myg


triggerTimeEvent :: UTCTime -> StateT Multi IO ()
triggerTimeEvent t = do
   gs <- gets games
   gs' <- lift $ mapM (\g -> trig t g `catch` timeExceptionHandler t g) gs
   modify(\m -> m{games = gs'})


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

