{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module implements the events that can affect a game.
module Nomyx.Core.Engine.GameEvents where

import Prelude hiding (log)
import Data.List
import Data.Time
import Data.Maybe
import Control.Monad.State
import Control.Lens
import Control.Category ((>>>))
import Control.Exception as E
import System.Log.Logger
import Nomyx.Language.Types
import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.EvalUtils
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Interpret
import Imprevu.Evaluation

-- | perform a game event
enactEvent :: GameEvent -> Maybe Rule -> StateT Game IO ()
enactEvent (JoinGame pn name) _                 = joinGame name pn
enactEvent (LeaveGame pn) _                     = leaveGame pn
enactEvent (ProposeRuleEv Propose pn rt ms)  mr = proposeRule rt ms pn mr
enactEvent (ProposeRuleEv SystemAdd _ rt ms) mr = systemAddRule rt ms mr
enactEvent (ProposeRuleEv Check _ _ _)       _  = return ()
enactEvent (ProposeRuleEv _ _ _ _) Nothing      = error "ProposeRuleEv: interpreter function needed"
enactEvent (InputResult pn en is id) _          = inputResult pn en is id
enactEvent (GLog mpn s) _                       = logGame s mpn
enactEvent (TimeEvent t) _                      = timeEvent t

enactTimedEvent :: Maybe Rule -> TimedEvent -> StateT Game IO ()
enactTimedEvent mr (TimedEvent t ge) = flip stateCatch updateError $ do
   currentTime .= t
   enactEvent ge mr
   lg <- get
   lift $ evaluate lg
   return ()

updateError :: SomeException -> StateT Game IO ()
updateError e = do
   warn 0 $ "IO error: " ++ show e
   logGame ("IO error: " ++ show e) Nothing

execGameEvent :: GameEvent -> StateT LoggedGame IO ()
execGameEvent = execGameEvent' Nothing

execGameEvent' :: Maybe Rule -> GameEvent -> StateT LoggedGame IO ()
execGameEvent' inter ge = do
   t <- use (game . currentTime)
   let te = TimedEvent t ge
   gameLog %= \gl -> gl ++ [te]
   zoom game $ enactTimedEvent inter te

getLoggedGame :: Game -> [TimedEvent] -> IO LoggedGame
getLoggedGame g tes = do
   let a = mapM_ (enactTimedEvent Nothing) tes
   g' <- execStateT a g
   return $ LoggedGame g' tes

-- | join the game.
joinGame :: PlayerName -> PlayerNumber -> StateT Game IO ()
joinGame name pn = do
   g <- get
   case find ((== pn) . getL playerNumber) (_players g) of
      Just _ -> return ()
      Nothing -> do
         info pn $ "Joining game: " ++ _gameName g
         let player = PlayerInfo { _playerNumber = pn, _playerName = name, _playingAs = Nothing}
         players %= (player : )
         mapStateIO $ runSystemEval pn $ triggerEvent (Signal Arrive) player


-- | leave the game.
leaveGame :: PlayerNumber -> StateT Game IO ()
leaveGame pn = mapStateIO $ runSystemEval pn $ void $ evDelPlayer pn

-- | insert a rule in pending rules.
proposeRule :: RuleTemplate -> [ModuleInfo] -> PlayerNumber -> Maybe Rule -> StateT Game IO ()
proposeRule rt ms pn mr = do
   rule <- createRule rt ms pn mr
   mapStateIO $ runEvalError (_rNumber rule) (Just pn) $ do --bug here
      r <- evProposeRule rule
      tracePN pn $ if r then "Your rule has been added to pending rules."
                        else "Error: Rule could not be proposed"
--   r <- mapStateIO $ runEvalError (_rNumber rule) (Just pn) (evProposeRule rule)  --do --bug here
--   if r then info pn "Your rule has been added to pending rules."
--        else warn pn "Error: Rule could not be proposed"

-- | add a rule forcefully (no votes etc.)
systemAddRule :: RuleTemplate -> [ModuleInfo] -> Maybe Rule -> StateT Game IO ()
systemAddRule rt ms mr = do
   rule <- createRule rt ms 0 mr
   let sysRule = (rStatus .~ Active) >>> (rAssessedBy .~ Just 0)
   rules %= (sysRule rule : )
   mapStateIO $ runEvalError (_rNumber rule) Nothing $ evalNomex (_rRule rule)

-- | insert a log message.
logGame :: String -> Maybe PlayerNumber -> StateT Game IO ()
logGame s mpn = do
   time <- use currentTime
   void $ logs %= (Log mpn time s : )

-- | the user has provided an input result
inputResult :: PlayerNumber -> EventNumber -> Input -> InputData -> StateT Game IO ()
inputResult pn en is id = do
   info pn $ "input result: input " ++ (show is) ++ " input data " ++ (show id)
   evs <- gets _events
   let rn = _erRuleNumber $ fromJust $ find (\(RuleEventInfo rn (EventInfo en' _ _ _ _)) -> en' == en) evs
   mapStateIO $ runEvalError rn (Just pn) $ triggerInput is id


timeEvent :: UTCTime -> StateT Game IO ()
timeEvent t = mapStateIO $ runSystemEval' $ evTriggerTime t

-- | A helper function to run the game state.
-- It additionally sets the current time
execWithGame :: UTCTime -> State LoggedGame () -> LoggedGame -> LoggedGame
execWithGame t gs g = execState gs $ set (game . currentTime) t g

execWithGame' :: UTCTime -> StateT LoggedGame IO () -> LoggedGame -> IO LoggedGame
execWithGame' t gs g = execStateT gs $ set (game . currentTime) t g

activeRules :: Game -> [RuleInfo]
activeRules = sort . filter ((==Active) . getL rStatus) . _rules

pendingRules :: Game -> [RuleInfo]
pendingRules = sort . filter ((==Pending) . getL rStatus) . _rules

rejectedRules :: Game -> [RuleInfo]
rejectedRules = sort . filter ((==Reject) . getL rStatus) . _rules

createRule :: RuleTemplate -> [ModuleInfo] -> PlayerNumber -> Maybe Rule -> StateT Game IO RuleInfo
createRule (RuleTemplate name des code _ _ _ decls) ms pn mr = do
   rs <- use rules
   let rn = getFreeNumber $ map _rNumber rs
   g <- get
   r <- case mr of
      -- If the rule has already been interpreted, use it
      Just r -> return r
      -- re-interpret the rule (in the case of loading game from logs)
      Nothing -> lift $ interpretRule' code ms
   info pn $ "Creating rule n=" ++ show rn ++ " code=" ++ code
   let ruleTemplate = RuleTemplate {_rName = name,
                                    _rDescription = des,
                                    _rRuleCode = code,
                                    _rAuthor = "Player " ++ (show pn),
                                    _rPicture = Nothing,
                                    _rCategory = [],
                                    _rDeclarations = map _modPath ms}
   return RuleInfo {_rNumber = rn,
                    _rProposedBy = pn,
                    _rRule = r,
                    _rStatus = Pending,
                    _rAssessedBy = Nothing,
                    _rRuleTemplate = ruleTemplate,
                    _rModules = map _modContent ms}

stateCatch :: Exception e => StateT Game IO a -> (e -> StateT Game IO a) -> StateT Game IO a
stateCatch m h = StateT $ \s -> runStateT m s `E.catch` \e -> runStateT (h e) s

getEventInfo :: EventNumber -> LoggedGame -> EventInfo
getEventInfo en g = _erEventInfo $ fromJust $ find ((== en) . getL (erEventInfo . eventNumber)) (_events $ _game g)

warn, info :: (MonadIO m) => Int -> String -> m ()
info pn s = liftIO $ infoM "Nomyx.Core.GameEvent" ("Player " ++ (show pn) ++ " " ++ s)
warn pn s = liftIO $ warningM "Nomyx.Core.GameEvent" ("Player " ++ (show pn) ++ " " ++ s)
