{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module implements the events that can affect a game.
module Nomyx.Core.Engine.GameEvents where

import Prelude hiding (log)
import Control.Monad.State
import Data.List
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.EventEval
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Utils
import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.InputEval
import Control.Lens
import Control.Category ((>>>))
import Control.Exception as E
import Data.Time
import Data.Maybe
import Data.Either

-- | perform a game event
enactEvent :: GameEvent -> Maybe (Either InterpretRule Rule) -> StateT Game IO ()
enactEvent (JoinGame pn name) _               = mapStateIO $ joinGame name pn
enactEvent (LeaveGame pn) _                   = mapStateIO $ leaveGame pn
enactEvent (ProposeRuleEv Propose pn rt ms)  (Just inter) = void $ proposeRule rt ms pn inter
enactEvent (ProposeRuleEv SystemAdd _ rt ms) (Just inter) = systemAddRule rt ms inter
enactEvent (ProposeRuleEv Check _ _ _)       _ = return ()
enactEvent (ProposeRuleEv _ _ _ _) Nothing               = error "ProposeRuleEv: interpreter function needed"
enactEvent (InputResult pn en fa ft ir) _     = mapStateIO $ inputResult pn en fa ft ir
enactEvent (GLog mpn s) _                     = mapStateIO $ logGame s mpn
enactEvent (TimeEvent t) _                    = mapStateIO $ runSystemEval' $ evTriggerTime t

enactTimedEvent :: Maybe (Either InterpretRule Rule) -> TimedEvent -> StateT Game IO ()
enactTimedEvent inter (TimedEvent t ge) = flip stateCatch updateError $ do
   currentTime .= t
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

execGameEvent' :: Maybe (Either InterpretRule Rule) -> GameEvent -> StateT LoggedGame IO ()
execGameEvent' inter ge = do
   t <- use (game . currentTime)
   let te = TimedEvent t ge
   gameLog %= \gl -> gl ++ [te]
   zoom game $ enactTimedEvent inter te

getLoggedGame :: Game -> InterpretRule -> [TimedEvent] -> IO LoggedGame
getLoggedGame g inter tes = do
   let a = mapM_ (enactTimedEvent (Just $ Left inter)) tes
   g' <- execStateT a g
   return $ LoggedGame g' tes

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
         runSystemEval pn $ triggerEvent (Signal Arrive) player


-- | leave the game.
leaveGame :: PlayerNumber -> State Game ()
leaveGame pn = runSystemEval pn $ void $ evDelPlayer pn

-- | insert a rule in pending rules.
proposeRule :: RuleTemplate -> [ModuleInfo] -> PlayerNumber -> Either InterpretRule Rule -> StateT Game IO ()
proposeRule rt ms pn inter = do
   rule <- createRule rt ms pn inter
   mapStateIO $ runEvalError (_rNumber rule) (Just pn) $ do
      r <- evProposeRule rule
      tracePN pn $ if r then "Your rule has been added to pending rules."
                        else "Error: Rule could not be proposed"

-- | add a rule forcefully (no votes etc.)
systemAddRule :: RuleTemplate -> [ModuleInfo] -> Either InterpretRule Rule -> StateT Game IO ()
systemAddRule rt ms inter = do
   rule <- createRule rt ms 0 inter
   let sysRule = (rStatus .~ Active) >>> (rAssessedBy .~ Just 0)
   rules %= (sysRule rule : )
   mapStateIO $ runEvalError (_rNumber rule) Nothing $ evalNomex (_rRule rule)

-- | insert a log message.
logGame :: String -> Maybe PlayerNumber -> State Game ()
logGame s mpn = do
   time <- use currentTime
   void $ logs %= (Log mpn time s : )

-- | the user has provided an input result
inputResult :: PlayerNumber -> EventNumber -> SignalAddress -> InputView -> InputDataView -> State Game ()
inputResult pn en sa ff ide = do
   tracePN pn $ "input result: EventNumber " ++ show en ++ ", SignalAddress " ++ show sa ++ ", Form " ++ show ff ++ ", choice " ++ show ide
   runSystemEval pn $ triggerInput ff ide sa en

--getGameTimes :: Game -> [UTCTime]
--getGameTimes g = concatMap (\ei -> getTimes ei g) (map _erEventInfo $ _events g)

--getTimes :: EventInfo -> Game -> [UTCTime]
--getTimes ei g = mapMaybe getTime (map snd $ getRemainingSignals ei g)
--
--getTime :: SomeSignal -> Maybe UTCTime
--getTime (SomeSignal t) = Just t
--getTime _                    = Nothing

-- | A helper function to run the game state.
-- It additionally sets the current time.
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

createRule :: RuleTemplate -> [ModuleInfo] -> PlayerNumber -> Either InterpretRule Rule -> StateT Game IO RuleInfo
createRule (RuleTemplate name des code _ _ _ decls) ms pn ei = do
   rs <- use rules
   let rn = getFreeNumber $ map _rNumber rs
   g <- get
   r <- case ei of
          -- If the rule has already been interpreted, use it
      Right r -> return r
      -- re-interpret the rule (in the case of loading game from logs)
      Left inter -> lift $ inter code ms
   tracePN pn $ "Creating rule n=" ++ show rn ++ " code=" ++ code
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
