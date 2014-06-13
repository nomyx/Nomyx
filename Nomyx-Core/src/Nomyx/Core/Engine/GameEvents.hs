{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module implements the events that can affect a game.
module Nomyx.Core.Engine.GameEvents where

import Prelude hiding (log)
import Control.Monad.State
import Data.List
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.Game
import Nomyx.Core.Engine.Utils
import Data.Lens
import Control.Category ((>>>))
import Data.Lens.Template
import Control.Exception as E
import Data.Time
import Data.Maybe

-- | a list of possible events affecting a game
data GameEvent = GameSettings      GameName GameDesc UTCTime
               | JoinGame          PlayerNumber PlayerName
               | LeaveGame         PlayerNumber
               | ProposeRuleEv     PlayerNumber SubmitRule
               | InputResult       PlayerNumber EventNumber InputNumber InputData
               | GLog              (Maybe PlayerNumber) String
               | TimeEvent         UTCTime
               | SystemAddRule     SubmitRule
                 deriving (Show, Read, Eq, Ord)

data TimedEvent = TimedEvent UTCTime GameEvent deriving (Show, Read, Eq, Ord)

-- | A game being non serializable, we have to store events in parralel in order to rebuild the state latter.
data LoggedGame = LoggedGame { _game :: Game,
                               _gameLog :: [TimedEvent]}
                               deriving (Show)

instance Eq LoggedGame where
   (LoggedGame {_game=g1}) == (LoggedGame {_game=g2}) = g1 == g2

instance Ord LoggedGame where
   compare (LoggedGame {_game=g1}) (LoggedGame {_game=g2}) = compare g1 g2

$( makeLens ''LoggedGame)

-- | perform a game event
enactEvent :: GameEvent -> Maybe (RuleCode -> IO Rule) -> StateT Game IO ()
enactEvent (GameSettings name desc date) _    = mapStateIO $ gameSettings name desc date
enactEvent (JoinGame pn name) _               = mapStateIO $ joinGame name pn
enactEvent (LeaveGame pn) _                   = mapStateIO $ leaveGame pn
enactEvent (ProposeRuleEv pn sr) (Just inter) = void $ proposeRule sr pn inter
enactEvent (InputResult pn en inn ir) _       = mapStateIO $ inputResult pn en inn ir
enactEvent (GLog mpn s) _                     = mapStateIO $ logGame s mpn
enactEvent (TimeEvent t) _                    = mapStateIO $ runEvalError Nothing $ evTriggerTime t
enactEvent (SystemAddRule r) (Just inter)     = systemAddRule r inter
enactEvent (ProposeRuleEv _ _) Nothing        = error "ProposeRuleEv: interpreter function needed"
enactEvent (SystemAddRule _) Nothing          = error "SystemAddRule: interpreter function needed"

enactTimedEvent :: Maybe (RuleCode -> IO Rule) -> TimedEvent -> StateT Game IO ()
enactTimedEvent inter (TimedEvent t ge) = flip stateCatch updateError $ do
   currentTime ~= t
   enactEvent ge inter
   lg <- get
   lift $ evaluate lg
   return ()

updateError :: SomeException -> StateT Game IO ()
updateError e = do
   liftIO $ putStrLn $ "IO error: " ++ show e
   mapStateIO $ logGame ("IO error: " ++ show e) Nothing

execGameEvent :: GameEvent -> StateT LoggedGame IO ()
execGameEvent = execGameEvent' Nothing

execGameEvent' :: Maybe (RuleCode -> IO Rule) -> GameEvent -> StateT LoggedGame IO ()
execGameEvent' inter ge = do
   t <- access $ game >>> currentTime
   let te = TimedEvent t ge
   gameLog %= \gl -> gl ++ [te]
   focus game $ enactTimedEvent inter te

getLoggedGame :: Game -> (RuleCode -> IO Rule) -> [TimedEvent] -> IO LoggedGame
getLoggedGame g mInter tes = do
   let a = mapM_ (enactTimedEvent (Just mInter)) tes
   g' <- execStateT a g
   return $ LoggedGame g' tes

-- | initialize the game.
gameSettings :: GameName -> GameDesc -> UTCTime -> State Game ()
gameSettings name desc date = do
   gameName ~= name
   gameDesc ~= desc
   currentTime ~= date
   return ()

-- | join the game.
joinGame :: PlayerName -> PlayerNumber -> State Game ()
joinGame name pn = do
   g <- get
   case find ((== pn) . getL playerNumber) (_players g) of
      Just _ -> return ()
      Nothing -> do
         tracePN pn $ "Joining game: " ++ _gameName g
         let player = PlayerInfo { _playerNumber = pn, _playerName = name, _playAs = Nothing}
         players %= (player : )
         runEvalError (Just pn) $ triggerEvent (Player Arrive) player


-- | leave the game.
leaveGame :: PlayerNumber -> State Game ()
leaveGame pn = runEvalError (Just pn) $ void $ evDelPlayer pn

-- | insert a rule in pending rules.
proposeRule :: SubmitRule -> PlayerNumber -> (RuleCode -> IO Rule) -> StateT Game IO ()
proposeRule sr pn inter = do
   rule <- createRule sr pn inter
   mapStateIO $ runEvalError (Just pn) $ do
      r <- evProposeRule rule
      tracePN pn $ if r then "Your rule has been added to pending rules."
                        else "Error: Rule could not be proposed"

-- | add a rule forcefully (no votes etc.)
systemAddRule :: SubmitRule -> (RuleCode -> IO Rule) -> StateT Game IO ()
systemAddRule sr inter = do
   rule <- createRule sr 0 inter
   let sysRule = (rStatus ^= Active) >>> (rAssessedBy ^= Just 0)
   rules %= (sysRule rule : )
   mapStateIO $ runEvalError (Just 0) $ void $ evalNomex (_rRule rule) (_rNumber rule)

-- | insert a log message.
logGame :: String -> Maybe PlayerNumber -> State Game ()
logGame s mpn = do
   time <- access currentTime
   void $ logs %= (Log mpn time s : )

-- | the user has provided an input result
-- TODO: this is relying on the EventNumber, which may change at all time
inputResult :: PlayerNumber -> EventNumber -> InputNumber -> InputData -> State Game ()
inputResult pn en inn ir = do
   tracePN pn $ "input result: EventNumber " ++ show en ++ ", InputNumber " ++ show inn ++ ", choice " ++ show ir
   runEvalError (Just pn) $ triggerInput en inn ir

getTimes :: EventInfo -> [UTCTime]
getTimes (EventInfo _ _ es _ SActive esr) = mapMaybe getTime (getEventFields es esr)
getTimes _ = []

getTime :: SomeField -> Maybe UTCTime
getTime (SomeField (Time t)) = Just t
getTime _                    = Nothing

-- | A helper function to use the state transformer GameState.
-- It additionally sets the current time.
execWithGame :: UTCTime -> State LoggedGame () -> LoggedGame -> LoggedGame
execWithGame t gs g = execState gs $ (game >>> currentTime) `setL` t $ g

execWithGame' :: UTCTime -> StateT LoggedGame IO () -> LoggedGame -> IO LoggedGame
execWithGame' t gs g = execStateT gs ((game >>> currentTime) `setL` t $ g)


activeRules :: Game -> [RuleInfo]
activeRules = sort . filter ((==Active) . getL rStatus) . _rules

pendingRules :: Game -> [RuleInfo]
pendingRules = sort . filter ((==Pending) . getL rStatus) . _rules

rejectedRules :: Game -> [RuleInfo]
rejectedRules = sort . filter ((==Reject) . getL rStatus) . _rules


createRule :: SubmitRule -> PlayerNumber -> (RuleCode -> IO Rule) -> StateT Game IO RuleInfo
createRule (SubmitRule name desc code) pn inter = do
   rs <- access rules
   let rn = getFreeNumber $ map _rNumber rs
   rf <- lift $ inter code
   tracePN pn $ "Creating rule n=" ++ show rn ++ " code=" ++ code
   return RuleInfo {_rNumber = rn,
                    _rName = name,
                    _rDescription = desc,
                    _rProposedBy = pn,
                    _rRuleCode = code,
                    _rRule = rf,
                    _rStatus = Pending,
                    _rAssessedBy = Nothing}

stateCatch :: Exception e => StateT Game IO a -> (e -> StateT Game IO a) -> StateT Game IO a
stateCatch m h = StateT $ \s -> runStateT m s `E.catch` \e -> runStateT (h e) s

getEventInfo :: EventNumber -> LoggedGame -> EventInfo
getEventInfo en g = fromJust $ find ((== en) . getL eventNumber) (_events $ _game g)
