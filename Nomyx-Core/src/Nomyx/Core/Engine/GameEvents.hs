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
import Control.Lens
import Control.Category ((>>>))
import Control.Exception as E
import Data.Time
import Data.Maybe

-- | perform a game event
enactEvent :: GameEvent -> Maybe InterpretRule -> StateT Game IO ()
enactEvent (JoinGame pn name) _               = mapStateIO $ joinGame name pn
enactEvent (LeaveGame pn) _                   = mapStateIO $ leaveGame pn
enactEvent (ProposeRuleEv pn sr) (Just inter) = void $ proposeRule sr pn inter
enactEvent (InputResult pn en fa ft ir) _     = mapStateIO $ inputResult pn en fa ft ir
enactEvent (GLog mpn s) _                     = mapStateIO $ logGame s mpn
enactEvent (TimeEvent t) _                    = mapStateIO $ runSystemEval' $ evTriggerTime t
enactEvent (SystemAddRule r) (Just inter)     = systemAddRule r inter
enactEvent (ProposeRuleEv _ _) Nothing        = error "ProposeRuleEv: interpreter function needed"
enactEvent (SystemAddRule _) Nothing          = error "SystemAddRule: interpreter function needed"

enactTimedEvent :: Maybe InterpretRule -> TimedEvent -> StateT Game IO ()
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

execGameEvent' :: Maybe InterpretRule -> GameEvent -> StateT LoggedGame IO ()
execGameEvent' inter ge = do
   t <- use (game . currentTime)
   let te = TimedEvent t ge
   gameLog %= \gl -> gl ++ [te]
   zoom game $ enactTimedEvent inter te

getLoggedGame :: Game -> InterpretRule -> [TimedEvent] -> IO LoggedGame
getLoggedGame g mInter tes = do
   let a = mapM_ (enactTimedEvent (Just mInter)) tes
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
         runSystemEval pn $ triggerEvent (Player Arrive) player


-- | leave the game.
leaveGame :: PlayerNumber -> State Game ()
leaveGame pn = runSystemEval pn $ void $ evDelPlayer pn

-- | insert a rule in pending rules.
proposeRule :: RuleTemplate -> PlayerNumber -> InterpretRule -> StateT Game IO ()
proposeRule rt pn inter = do
   rule <- createRule rt pn inter
   mapStateIO $ runEvalError (_rNumber rule) (Just pn) $ do
      r <- evProposeRule rule
      tracePN pn $ if r then "Your rule has been added to pending rules."
                        else "Error: Rule could not be proposed"

-- | add a rule forcefully (no votes etc.)
systemAddRule :: RuleTemplate -> InterpretRule -> StateT Game IO ()
systemAddRule rt inter = do
   rule <- createRule rt 0 inter
   let sysRule = (rStatus .~ Active) >>> (rAssessedBy .~ Just 0)
   rules %= (sysRule rule : )
   mapStateIO $ runEvalError (_rNumber rule) Nothing $ evalNomex (_rRule rule)

-- | insert a log message.
logGame :: String -> Maybe PlayerNumber -> State Game ()
logGame s mpn = do
   time <- use currentTime
   void $ logs %= (Log mpn time s : )

-- | the user has provided an input result
inputResult :: PlayerNumber -> EventNumber -> SignalAddress -> FormField -> InputData -> State Game ()
inputResult pn en sa ff ide = do
   tracePN pn $ "input result: EventNumber " ++ show en ++ ", SignalAddress " ++ show sa ++ ", Form " ++ show ff ++ ", choice " ++ show ide
   runSystemEval pn $ triggerInput ff ide sa en

getGameTimes :: Game -> [UTCTime]
getGameTimes g = concatMap (\ei -> getTimes ei g) (_events g)

getTimes :: EventInfo -> Game -> [UTCTime]
getTimes ei g = mapMaybe getTime (map snd $ getRemainingSignals ei g)

getTime :: SomeSignal -> Maybe UTCTime
getTime (SomeSignal (Time t)) = Just t
getTime _                    = Nothing

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

createRule :: RuleTemplate -> PlayerNumber -> InterpretRule -> StateT Game IO RuleInfo
createRule (RuleTemplate name des code _ _ _ decls) pn inter = do
   rs <- use rules
   let rn = getFreeNumber $ map _rNumber rs
   rf <- lift $ inter code
   tracePN pn $ "Creating rule n=" ++ show rn ++ " code=" ++ code
   let ruleTemplate = RuleTemplate {_rName = name,
                                    _rDescription = des,
                                    _rRuleCode = code,
                                    _rAuthor = "Player " ++ (show pn),
                                    _rPicture = Nothing,
                                    _rCategory = [],
                                    _rDeclarations = []}
   return RuleInfo {_rNumber = rn,
                    _rProposedBy = pn,
                    _rRule = rf,
                    _rStatus = Pending,
                    _rAssessedBy = Nothing,
                    _rRuleTemplate = ruleTemplate}

stateCatch :: Exception e => StateT Game IO a -> (e -> StateT Game IO a) -> StateT Game IO a
stateCatch m h = StateT $ \s -> runStateT m s `E.catch` \e -> runStateT (h e) s

getEventInfo :: EventNumber -> LoggedGame -> EventInfo
getEventInfo en g = fromJust $ find ((== en) . getL eventNumber) (_events $ _game g)
