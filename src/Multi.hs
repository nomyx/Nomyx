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
import Language.Haskell.Interpreter.Server
import Language.Nomyx.Evaluation
import Control.Concurrent.STM
import Data.Maybe
import Control.Concurrent
import System.Posix.Resource
import Types
import Control.Applicative
import Control.Exception

-- | helper function to change a player's ingame status.
mayJoinGame :: Maybe GameName -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
mayJoinGame maybename pn pl = case find (\(PlayerMulti mypn _ _ _ _ _) -> mypn == pn) pl of
                     Just o -> replace o o{ inGame = maybename} pl
                     Nothing -> pl

newPlayerU :: PlayerMulti -> StateT Multi IO ()
newPlayerU pm = do
   pms <- gets mPlayers
   modify (\multi -> multi { mPlayers = pm : pms})


getNewPlayerNumber :: StateT Multi IO PlayerNumber
getNewPlayerNumber = do
   ps <- gets mPlayers
   return $ length ps + 1


addNewGame :: Game -> StateT Multi IO ()
addNewGame new = modify (\multi@Multi {games=gs} -> multi {games =  new:gs})

getGameByName :: GameName -> StateT Multi IO (Maybe Game)
getGameByName gn =  fmap (find (\(Game {gameName = n}) -> n==gn)) (gets games)

joinGamePlayer :: PlayerNumber -> GameName -> StateT Multi IO ()
joinGamePlayer pn game = modify (\multi -> multi {mPlayers = mayJoinGame (Just game) pn (mPlayers multi)})


leaveGameU :: PlayerNumber -> StateT Multi IO ()
leaveGameU pn = modify (\multi -> multi {mPlayers = mayJoinGame Nothing pn (mPlayers multi)})

-- | list the active games
listGame :: PlayerNumber -> StateT Multi IO ()
listGame _ = do
   gs <- gets games
   case length gs of
      0 -> say "No active games"
      _ -> do
         say "Active games:"
         say $ concatMap (\g -> gameName g ++ "\n") gs

-- | starts a new game
newGame :: String -> PlayerNumber -> StateT Multi IO ()
newGame name _ = do
   gs <- gets games
   case null $ filter (\p -> gameName p == name) gs of
      True -> do
         say $ "Creating a new game of name: " ++ name
         t <- liftIO getCurrentTime
         -- create a game with zero players
         modify (\m -> m {games = (initialGame name t):gs})
      False -> say $ "this name is already used"

uniqueGame :: String -> [Game] -> Bool
uniqueGame s gs = null $ filter (\p -> gameName p == s) gs

-- | join a game.
joinGame :: GameName -> PlayerNumber -> StateT Multi IO ()
joinGame game pn = do
   mg <- getGameByName game
   case mg of
      Nothing -> say $ "No game by that name"
      Just g -> do
         say "subscribing first."
         subscribeGame (gameName g) pn
         say $ "Joining game: " ++ game
         joinGamePlayer pn game


-- | leave a game (you remain subscribed).
leaveGame :: PlayerNumber -> StateT Multi IO  ()
leaveGame pn = do
   leaveGameU pn
   say "You left the game (you remain subscribed)."


-- | subcribe to a game.
subscribeGame :: GameName -> PlayerNumber -> StateT Multi IO ()
subscribeGame game pn = do
   m <- get
   inGameDo game $ do
      g <- get
      case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
         Just _ -> say "Already subscribed!"
         Nothing -> do
            say $ "Subscribing to game: " ++ game
            let player = PlayerInfo { playerNumber = pn, playerName = getPlayersName pn m}
            put g {players = player : (players g)}
            liftT $ triggerEvent (Player Arrive) (PlayerData player)


-- | subcribe to a game.
unsubscribeGame :: GameName -> PlayerNumber -> StateT Multi IO ()
unsubscribeGame game pn = inGameDo game $ do
   g <- get
   case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
      Nothing -> say "Not subscribed!"
      Just _ -> do
         say $ "Unsubscribing to game: " ++ game
         let player = PlayerInfo { playerNumber = pn, playerName = getPlayersName' g pn}
         put g {players = filter (\PlayerInfo { playerNumber = mypn} -> mypn /= pn) (players g)}
         liftT $ triggerEvent (Player Leave) (PlayerData player)


showSubGame :: GameName -> PlayerNumber -> StateT Multi IO  ()
showSubGame g _ = inGameDo g $ do
   ps <- gets players
   say $ concatMap show ps

showSubscribtion :: PlayerNumber -> StateT Multi IO  ()
showSubscribtion pn = inPlayersGameDo_ pn $ do
   ps <- gets players
   say $ concatMap show ps


-- | insert a rule in pending rules.
submitRule :: SubmitRule -> PlayerNumber -> ServerHandle -> StateT Multi IO ()
submitRule sr pn sh = do
   mnr <- enterRule sr pn sh
   case mnr of
      Just nr -> do
         inPlayersGameDo pn $ do
            r <- liftT $ evProposeRule nr
            if r == True then say $ "Your rule has been added to pending rules."
            else say $ "Error: Rule could not be proposed"
         updateLastRule Nothing pn
      Nothing -> updateLastRule (Just sr) pn


-- | reads a rule.
enterRule :: SubmitRule -> PlayerNumber -> ServerHandle -> StateT Multi IO (Maybe Rule)
enterRule (SubmitRule name desc code) pn sh = join <$> (inPlayersGameDo pn $ do
   rs <- gets rules
   let rn = getFreeNumber $ map rNumber rs
   mrr <- lift $ interpretRule code sh
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

updateLastRule :: Maybe SubmitRule -> PlayerNumber -> StateT Multi IO ()
updateLastRule msr pn = do
   pm <- fromJust <$> findPlayer' pn
   pls <- gets mPlayers
   let pls' = replace pm (pm {lastRule = msr}) pls
   modify (\m -> m{mPlayers = pls'})

cpuTimeLimitSoft = ResourceLimit 4
cpuTimeLimitHard = ResourceLimit 5
limits :: [(Resource, ResourceLimits)]
limits = [ (ResourceCPUTime,      ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)]

inputChoiceResult :: EventNumber -> Int -> PlayerNumber -> StateT Multi IO  ()
inputChoiceResult eventNumber choiceIndex pn = inPlayersGameDo_ pn $ liftT $ triggerChoice eventNumber choiceIndex

-- TODO maybe homogeneise both inputs event
inputStringResult :: Event InputString -> String -> PlayerNumber -> StateT Multi IO  ()
inputStringResult event input pn = inPlayersGameDo_ pn $ liftT $ triggerEvent event (InputStringData input)

inputUpload :: PlayerNumber -> FilePath -> String -> ServerHandle -> StateT Multi IO  ()
inputUpload pn dir mod sh = inPlayersGameDo_ pn $ do
    m <- lift $ loadModule dir mod sh
    case m of
      Right _ -> do
         output ("File loaded: " ++ show dir ++ " Module " ++ show mod ++"\n") pn
         return ()
      Left e -> do
         output ("Compiler error: " ++ show e ++ "\n") pn
         return ()

output :: String -> PlayerNumber -> StateT Game IO ()
output s pn = modify (\game -> game { outputs = (pn, s) : (outputs game)})

outputAll :: String -> StateT Game IO ()
outputAll s = gets players >>= mapM_ ((output s) . playerNumber)

mailSettings :: MailSettings -> PlayerNumber -> StateT Multi IO ()
mailSettings mailSettings pn = do
   mps <- gets mPlayers
   case find (\(PlayerMulti {mPlayerNumber}) -> pn==mPlayerNumber) mps of
      Nothing -> say "settings not modified!"
      Just pm -> do
         let newmps = replace pm pm{mMail=mailSettings} mps
         modify (\m -> m{mPlayers = newmps})


-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: Game -> StateT Multi IO  ()
modifyGame g = do
   m@(Multi {games=gs}) <- get
   case find (\myg -> gameName g == gameName myg) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg g gs
         put (m {games=newgs})



-- | show the constitution.
showConstitution :: PlayerNumber -> StateT Multi IO ()
showConstitution pn = inPlayersGameDo_ pn $ get >>= (say  .  show  .  activeRules)


-- | show every rules (including pendings and deleted)
showAllRules :: PlayerNumber -> StateT Multi IO ()	
showAllRules pn = inPlayersGameDo_ pn $ get >>= (say . show . rules)

displayPlayer :: PlayerMulti -> String
displayPlayer (PlayerMulti pn name _ _ (Just game) _) = show pn ++ ": " ++ name ++ " in game: " ++ game ++ "\n"
displayPlayer (PlayerMulti pn name _ _ Nothing _)     = show pn ++ ": " ++ name ++ "\n"


-- | quit the game
quit :: PlayerNumber -> IO ()
quit _ = putStrLn "quit"

-- | Utility functions

-- | replace the player's name in the list
setName :: String -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
setName name pn pl = case find (\(PlayerMulti h _ _ _ _ _) -> h == pn) pl of
                        Just o -> replace o o{ mPlayerName = name} pl
                        Nothing -> pl



-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> StateT Game IO a -> StateT Multi IO (Maybe a)
inPlayersGameDo pn action = do
   multi <- get
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> say "You must be in a game" >> return Nothing
      Just g -> do
         (a, myg) <- lift $ runStateT action g
         modifyGame myg
         return (Just a)

inPlayersGameDo_ :: PlayerNumber -> StateT Game IO a -> StateT Multi IO ()
inPlayersGameDo_ pn action = inPlayersGameDo pn action >> return ()

inGameDo :: GameName -> StateT Game IO () -> StateT Multi IO ()
inGameDo game action = do
   gs <- gets games
   case find (\(Game {gameName =n}) -> n==game) gs of
      Nothing -> say "No game by that name"
      Just g -> do
         myg <- lift $ execWithGame action g
         modifyGame myg


triggerTimeEvent :: TVar Multi -> UTCTime -> IO()
triggerTimeEvent tm t = do
    m <- atomically $ readTVar tm
    gs' <- mapM (\g -> trig t g `catch` timeExceptionHandler g) (games m)
    let m' = m {games = gs'}
    atomically $ writeTVar tm m'

trig :: UTCTime -> Game -> IO Game
trig t g =  do
   g <- execWithGame t (liftT $ evTriggerTime t) g
   evaluate g
   return g

timeExceptionHandler :: Game -> ErrorCall -> IO Game
timeExceptionHandler g e = do
   putStrLn $ "Error in triggerTimeEvent: " ++ (show e)
   execWithGame (outputAll $ "Error while triggering a time event: " ++ (show e) ++
                           "\nThe event have been canceled. Please remove/fix the faulty rule.") g


-- | get all events that has not been triggered yet
getTimeEvents :: UTCTime -> TVar Multi -> IO([UTCTime])
getTimeEvents now tm = do
    m <- atomically $ readTVar tm
    let times = catMaybes $ map getTimes $ concatMap events $ games m
    return $ filter (\t -> t <= now) times

--TODO: fix this
launchTimeEvents :: TVar Multi -> IO()
launchTimeEvents tm = do
    now <- getCurrentTime
    putStrLn $ "tick " ++ (show now)
    schedule <- getTimeEvents now tm
    when (length schedule /= 0) $ putStrLn "found time event"
    mapM_ (triggerTimeEvent tm) schedule
    --sleep 1 second roughly
    threadDelay 1000000
    launchTimeEvents tm


getTimes :: EventHandler -> Maybe UTCTime
getTimes (EH _ _ (Time t) _) = Just t
getTimes _ = Nothing

