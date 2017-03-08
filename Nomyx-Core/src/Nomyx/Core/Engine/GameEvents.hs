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
import Nomyx.Core.Engine.Utils
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Interpret
import Imprevu.Evaluation hiding (runEvalError)

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
   case find ((== pn) . view playerNumber) (_players g) of
      Just _ -> return ()
      Nothing -> do
         let player = PlayerInfo { _playerNumber = pn, _playerName = name, _playingAs = Nothing}
         players %= (player : )
         void $ mapStateIO $ runSystemEval pn $ triggerEvent (Signal Arrive) player


-- | leave the game.
leaveGame :: PlayerNumber -> StateT Game IO ()
leaveGame pn = void $ mapStateIO $ runSystemEval pn $ void $ evDelPlayer pn

-- | insert a rule in pending rules.
proposeRule :: RuleTemplate -> [ModuleInfo] -> PlayerNumber -> Maybe Rule -> StateT Game IO ()
proposeRule rt ms pn mr = do
   rule <- createRule rt ms pn mr
   r <- mapStateIO $ runEvalError (_rNumber rule) (Just pn) (evProposeRule rule)  --do --bug here
   case r of
     Just _  -> info pn "Your rule has been added to pending rules."
     Nothing -> warn pn "Error: Rule could not be proposed"

-- | add a rule forcefully (no votes etc.)
systemAddRule :: RuleTemplate -> [ModuleInfo] -> Maybe Rule -> StateT Game IO ()
systemAddRule rt ms mr = do
   rule <- createRule rt ms 0 mr
   let sysRule = (rStatus .~ Active) >>> (rAssessedBy .~ Just 0)
   rules %= (sysRule rule : )
   void $ mapStateIO $ runEvalError (_rNumber rule) Nothing $ evalNomex (_rRule rule)

-- | insert a log message.
logGame :: String -> Maybe PlayerNumber -> StateT Game IO ()
logGame s mpn = do
   time <- use currentTime
   void $ logs %= (Log mpn time s : )

-- | the user has provided an input result
inputResult :: PlayerNumber -> EventNumber -> Input -> InputData -> StateT Game IO ()
inputResult pn en is id = do
   debug pn $ "input result: input " ++ (show is) ++ " input data " ++ (show id)
   evs <- gets _events
   let rn = _erRuleNumber $ fromJust $ find (\(RuleEventInfo rn (EventInfo en' _ _ _ _)) -> en' == en) evs
   void $ mapStateIO $ runEvalError rn (Just pn) $ triggerInput is id


timeEvent :: UTCTime -> StateT Game IO ()
timeEvent t = void $ mapStateIO $ runSystemEval' $ evTriggerTime t

-- | A helper function to run the game state.
-- It additionally sets the current time
execWithGame :: UTCTime -> State LoggedGame () -> LoggedGame -> LoggedGame
execWithGame t gs g = execState gs $ set (game . currentTime) t g

execWithGame' :: UTCTime -> StateT LoggedGame IO () -> LoggedGame -> IO LoggedGame
execWithGame' t gs g = execStateT gs $ set (game . currentTime) t g

activeRules :: Game -> [RuleInfo]
activeRules = sort . filter ((==Active) . view rStatus) . _rules

pendingRules :: Game -> [RuleInfo]
pendingRules = sort . filter ((==Pending) . view rStatus) . _rules

rejectedRules :: Game -> [RuleInfo]
rejectedRules = sort . filter ((==Reject) . view rStatus) . _rules

createRule :: RuleTemplate -> [ModuleInfo] -> PlayerNumber -> Maybe Rule -> StateT Game IO RuleInfo
createRule (RuleTemplate name des code author _ _ decls) ms pn mr = do
   rs <- use rules
   let rn = head $ [1..] \\ (map _rNumber rs)
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
                                    _rAuthor = author,
                                    _rPicture = Nothing,
                                    _rCategory = [],
                                    _rDeclarations = []} --Declarations are transfered in the RuleInfo
   return RuleInfo {_rNumber = rn,
                    _rProposedBy = pn,
                    _rRule = r,
                    _rStatus = Pending,
                    _rAssessedBy = Nothing,
                    _rRuleTemplate = ruleTemplate,
                    _rModules = ms}

stateCatch :: Exception e => StateT Game IO a -> (e -> StateT Game IO a) -> StateT Game IO a
stateCatch m h = StateT $ \s -> runStateT m s `E.catch` \e -> runStateT (h e) s

getEventInfo :: EventNumber -> LoggedGame -> EventInfo
getEventInfo en g = _erEventInfo $ fromJust $ find ((== en) . view (erEventInfo . eventNumber)) (_events $ _game g)

warn, info :: (MonadIO m) => Int -> String -> m ()
info  pn s = liftIO $ infoM "Nomyx.Core.GameEvent" ("Player " ++ (show pn) ++ " " ++ s)
warn  pn s = liftIO $ warningM "Nomyx.Core.GameEvent" ("Player " ++ (show pn) ++ " " ++ s)
debug pn s = liftIO $ debugM "Nomyx.Core.GameEvent" ("Player " ++ (show pn) ++ " " ++ s)
