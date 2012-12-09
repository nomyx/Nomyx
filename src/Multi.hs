{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators,
    TypeSynonymInstances, FlexibleInstances, GADTs #-}

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
import Data.Typeable
import Data.Function (on)
import Debug.Trace.Helpers()
import Language.Nomyx.Expression
import Data.Time
import Language.Haskell.Interpreter.Server
import Language.Nomyx.Evaluation
import Control.Concurrent.STM
import Data.Maybe
import Control.Concurrent


type PlayerPassword = String

data PlayerMulti = PlayerMulti   { mPlayerNumber :: PlayerNumber,
                                   mPlayerName :: PlayerName,
                                   mPassword :: PlayerPassword,
                                   inGame :: Maybe GameName}
                                   deriving (Eq, Show, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { games   :: [Game],
                     mPlayers :: [PlayerMulti]}
                     deriving (Eq, Typeable)

instance Show Multi where
   show Multi{games=gs, mPlayers=mps} = show (sort gs) ++ "\n" ++ show (sort mps)

defaultMulti :: Multi
defaultMulti = Multi [] []

-- | helper function to change a player's ingame status.
mayJoinGame :: Maybe GameName -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
mayJoinGame maybename pn pl = case find (\(PlayerMulti mypn _ _ _) -> mypn == pn) pl of
                     Just o -> replace o o{ inGame = maybename} pl
                     Nothing -> pl

newPlayerU :: PlayerMulti -> StateT Multi IO ()
newPlayerU pm = do
   pms <- gets mPlayers
   modify (\multi -> multi { mPlayers = pm : pms})

findPlayer :: PlayerName -> StateT Multi IO (Maybe PlayerMulti)
findPlayer name =  fmap (find (\PlayerMulti {mPlayerName = pn} -> pn==name)) (gets mPlayers)

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
            put g {players = PlayerInfo { playerNumber = pn,
                                          playerName = getPlayersName pn m} : (players g)}


-- | subcribe to a game.
unsubscribeGame :: GameName -> PlayerNumber -> StateT Multi IO ()
unsubscribeGame game pn = inGameDo game $ do
   g <- get
   case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
      Nothing -> say "Not subscribed!"
      Just _ -> do
         say $ "Unsubscribing to game: " ++ game
         put g {players = filter (\PlayerInfo { playerNumber = mypn} -> mypn /= pn) (players g)}


showSubGame :: GameName -> PlayerNumber -> StateT Multi IO  ()
showSubGame g _ = inGameDo g $ do
   ps <- gets players
   say $ concatMap show ps

showSubscribtion :: PlayerNumber -> StateT Multi IO  ()
showSubscribtion pn = inPlayersGameDo pn $ do
   ps <- gets players
   say $ concatMap show ps


-- | insert a rule in pending rules.
-- the rules are numbered incrementaly.
submitRule :: String -> String -> String -> PlayerNumber -> ServerHandle -> StateT Multi IO  ()
submitRule name text rule pn sh = inPlayersGameDo pn $ do
   --input the new rule (may fail if ill-formed)
   rs <- gets rules
   mnr <- enterRule (length rs + 1) name text rule pn sh
   case mnr of
      Just nr -> do
         r <- liftT $ evProposeRule nr
         if r == True then say $ "Your rule has been added to pending rules."
             else say $ "Error: Rule could not be proposed"
         return ()
      Nothing -> say $ "Please try again."

inputChoiceResult :: EventNumber -> Int -> PlayerNumber -> StateT Multi IO  ()
inputChoiceResult eventNumber choiceIndex pn = inPlayersGameDo pn $ liftT $ triggerChoice eventNumber choiceIndex

inputStringResult :: Event InputString -> String -> PlayerNumber -> StateT Multi IO  ()
inputStringResult event input pn = inPlayersGameDo pn $ liftT $ triggerEvent event (InputStringData input)

output :: PlayerNumber -> String -> StateT Game IO ()
output pn s = modify (\game -> game { outputs = (pn, s) : (outputs game)})

-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: Game -> StateT Multi IO  ()
modifyGame g = do
   Multi gs ps <- get
   case find (\myg -> gameName g == gameName myg) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg g gs
         put $ Multi newgs ps

-- | reads a rule.
enterRule :: RuleNumber -> String -> String -> String -> PlayerNumber -> ServerHandle -> StateT Game IO (Maybe Rule)
enterRule num name text ruleText pn sh = do
   mrr <- lift $ interpretRule ruleText sh
   case mrr of
      Right ruleFunc -> return $ Just Rule {rNumber = num,
                      rName = name,
                      rDescription = text,
                      rProposedBy = pn,
                      rRuleCode = ruleText,
                      rRuleFunc = ruleFunc,
                      rStatus = Pending,
                      rAssessedBy = Nothing}
      Left e -> do
         output pn $ "Compiler error: " ++ show e ++ "\n"
         return Nothing


-- | show the constitution.
showConstitution :: PlayerNumber -> StateT Multi IO ()
showConstitution pn = inPlayersGameDo pn $ get >>= (say  .  show  .  activeRules)


-- | show every rules (including pendings and deleted)
showAllRules :: PlayerNumber -> StateT Multi IO ()	
showAllRules pn = inPlayersGameDo pn $ get >>= (say . show . rules)

displayPlayer :: PlayerMulti -> String
displayPlayer (PlayerMulti pn name _ (Just game)) = show pn ++ ": " ++ name ++ " in game: " ++ game ++ "\n"
displayPlayer (PlayerMulti pn name _ Nothing)     = show pn ++ ": " ++ name ++ "\n"


-- | quit the game
quit :: PlayerNumber -> IO ()
quit _ = putStrLn "quit"

-- | Utility functions

-- | replace the player's name in the list
setName :: String -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
setName name pn pl = case find (\(PlayerMulti h _ _ _) -> h == pn) pl of
                        Just o -> replace o o{ mPlayerName = name} pl
                        Nothing -> pl


-- | returns the game the player is in						
getPlayersGame :: PlayerNumber -> Multi -> Maybe Game
getPlayersGame pn multi = do
        pi <- find (\(PlayerMulti n _ _ _) -> n==pn) (mPlayers multi)
        gn <- inGame pi
        find (\(Game {gameName=name}) -> name==gn) (games multi)

getPlayersName :: PlayerNumber -> Multi -> PlayerName
getPlayersName pn multi = do
   case find (\(PlayerMulti n _ _ _) -> n==pn) (mPlayers multi) of
      Nothing -> error "getPlayersName: No player by that number"
      Just pm -> mPlayerName pm

getPlayersName' :: Game -> PlayerNumber -> PlayerName
getPlayersName' g pn = do
   case find (\(PlayerInfo n _) -> n==pn) (players g) of
      Nothing -> error "getPlayersName: No player by that number in that game"
      Just pm -> playerName pm

-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> StateT Game IO () -> StateT Multi IO ()
inPlayersGameDo pn action = do
   multi <- get
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> say "You must be in a game"
      Just g -> do
         myg <- lift $ execWithGame action g
         modifyGame myg

inGameDo :: GameName -> StateT Game IO () -> StateT Multi IO ()
inGameDo game action = do
   gs <- gets games
   case find (\(Game {gameName =n}) -> n==game) gs of
      Nothing -> say "No game by that name"
      Just g -> do
         myg <- lift $ execWithGame action g
         modifyGame myg

instance Ord PlayerMulti where
  (<=) = (<=) `on` mPlayerNumber


triggerTimeEvent :: TVar Multi -> UTCTime -> IO()
triggerTimeEvent tm t = do
    m <- atomically $ readTVar tm
    gs' <- mapM (trig t) (games m)
    let m' = m {games = gs'}
    atomically $ writeTVar tm m'
       where trig t g =  execWithGame (liftT $ evTriggerTime t) g

execGame :: State Game () -> Game -> Game
execGame s g = execState s g

-- | get all events within time and time + 2 second
getTimeEvents :: UTCTime -> TVar Multi -> IO([UTCTime])
getTimeEvents time tm = do
    m <- atomically $ readTVar tm
    let times = catMaybes $ map getTimes $ concatMap events $ games m
    return $ filter (\t-> t >= time && t < addUTCTime 2 time) times

--TODO: fix this
launchTimeEvents :: TVar Multi -> IO()
launchTimeEvents tm = do
    now <- getCurrentTime
    --putStrLn $ "tick " ++ (show now)
    schedule <- getTimeEvents now tm
    when (length schedule /= 0) $ putStrLn "found time event"
    mapM_ (triggerTimeEvent tm) schedule
    after <- getCurrentTime
    --sleep 1 second minus rough delay of execution
    threadDelay $ truncate(1000000 - 1000000*(diffUTCTime after now))
    launchTimeEvents tm


getTimes :: EventHandler -> Maybe UTCTime
getTimes (EH _ _ (Time t) _) = Just t
getTimes _ = Nothing

