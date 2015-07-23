{-# LANGUAGE GADTs #-}

-- | Evaluation of a Nomyx expression
module Nomyx.Core.Engine.Evaluation where

import Prelude hiding ((.), log)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Typeable
import Data.Time
import Data.Maybe
import Data.Todo
import Control.Category hiding (id)
import Control.Applicative
import Control.Monad.Error
import Control.Lens
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Types hiding (_vRuleNumber)
import Nomyx.Core.Engine.EventEval
import Nomyx.Core.Engine.EvalUtils
import Nomyx.Core.Engine.Utils
import System.Random



-- * Evaluation

-- | evaluate an effecful expression.
evalNomex :: Nomex a -> Evaluate a
evalNomex (NewVar v a)            = evNewVar v a
evalNomex (DelVar v)              = evDelVar v
evalNomex (WriteVar v val)        = evWriteVar v val
evalNomex (OnEvent ev h)          = evOnEvent ev h
evalNomex (DelEvent en)           = evDelEvent en
evalNomex (SendMessage m d)       = evSendMessage m d
evalNomex (NewOutput pn s)        = evNewOutput pn s
evalNomex (UpdateOutput on s)     = evUpdateOutput on s
evalNomex (DelOutput on)          = evDelOutput on
evalNomex (ProposeRule rule)      = evProposeRule rule
evalNomex (ActivateRule rule)     = evActivateRule rule
evalNomex (RejectRule rule)       = evRejectRule rule
evalNomex (AddRule rule)          = evAddRule rule
evalNomex (ModifyRule mod rule)   = evModifyRule mod rule
evalNomex (SetPlayerName pn n)    = evChangeName pn n
evalNomex (DelPlayer pn)          = evDelPlayer pn
evalNomex (SetVictory ps)         = evSetVictory ps
evalNomex (LiftEffect e)          = liftEval $ evalNomexNE e
evalNomex (ThrowError s)          = throwError s
evalNomex (CatchError n h)        = catchError (evalNomex n) (\a -> evalNomex (h a))
evalNomex (Return a)              = return a
evalNomex (Bind exp f)            = evalNomex exp >>= \e -> evalNomex (f e)
evalNomex (GetRandomNumber r)     = evGetRandomNumber r

-- | evaluate an effectless expression.
evalNomexNE :: NomexNE a -> EvaluateNE a
evalNomexNE (ReadVar v)     = evReadVar v
evalNomexNE (GetOutput on)  = evGetOutput on
evalNomexNE  GetRules       = _rules <$> asks _eGame
evalNomexNE  GetPlayers     = _players <$> asks _eGame
evalNomexNE  GetEvents      = _events <$> asks _eGame
evalNomexNE  SelfRuleNumber = asks _eRuleNumber
evalNomexNE (CurrentTime)   = _currentTime <$> asks _eGame
evalNomexNE (Return a)      = return a
evalNomexNE (Bind exp f)    = evalNomexNE exp >>= \e -> evalNomexNE (f e)
evalNomexNE (Simu sim ev)   = evSimu sim ev


evNewVar :: (Typeable a, Show a) => VarName -> a -> Evaluate (Maybe (V a))
evNewVar name def = do
   (vars, rn) <- accessGame variables
   case find ((== name) . getL vName) vars of
      Nothing -> do
         modifyGame variables (Var rn name def : )
         return $ Just (V name)
      Just _ -> return Nothing

evDelVar :: (V a) -> Evaluate Bool
evDelVar (V name) = do
   (vars, _) <- accessGame variables
   case find ((== name) . getL vName) vars of
      Nothing -> return False
      Just _ -> do
         modifyGame variables $ filter ((/= name) . getL vName)
         return True

evWriteVar :: (Typeable a, Show a) => V a -> a -> Evaluate Bool
evWriteVar (V name) val = do
   (vars, _) <- accessGame variables
   case find (\(Var _ myName _) -> myName == name) vars of
      Nothing -> return False
      Just (Var rn myName _) -> do
         modifyGame variables $ replaceWith ((== name) . getL vName) (Var rn myName val)
         return True

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> Evaluate EventNumber
evOnEvent event handler = do
   (evs, rn) <- accessGame events
   let en = getFreeNumber (map _eventNumber evs)
   modifyGame events (EventInfo en rn event handler SActive [] : )
   return en

evSendMessage :: (Typeable a, Show a) => Msg a -> a -> Evaluate ()
evSendMessage (Msg id) d = triggerEvent (Message (Msg id)) d

evProposeRule :: RuleInfo -> Evaluate Bool
evProposeRule rule = do
   (rs, _) <- accessGame rules
   case find (\a -> (rule ^. rNumber) == (a ^. rNumber)) rs of
      Nothing -> do
         modifyGame rules (rule:)
         triggerEvent (RuleEv Proposed) rule
         return True
      Just _ -> return False

--Sets the rule status to Active and execute it if possible
evActivateRule :: RuleNumber -> Evaluate Bool
evActivateRule rn = do
   (rs, by) <- accessGame rules
   case find (\r -> _rNumber r == rn && _rStatus r /= Active) rs of
      Nothing -> return False
      Just r -> do
         putGame rules $ replaceWith ((== rn) . getL rNumber) r{_rStatus = Active, _rAssessedBy = Just by} rs
         --execute the rule
         withRN (_rNumber r) $ evalNomex (_rRule r)
         triggerEvent (RuleEv Activated) r
         return True

evRejectRule :: RuleNumber -> Evaluate Bool
evRejectRule rn = do
   (rs, by) <- accessGame rules
   case find (\r -> _rNumber r == rn && _rStatus r /= Reject) rs of
      Nothing -> return False
      Just r -> do
         delVarsRule rn
         delEventsRule rn
         delOutputsRule rn
         delVictoryRule rn
         putGame rules $ replaceWith ((== rn) . getL rNumber) r{_rStatus = Reject, _rAssessedBy = Just by} rs
         triggerEvent (RuleEv Rejected) r
         return True

evAddRule :: RuleInfo -> Evaluate Bool
evAddRule rule = do
   (rs, _) <- accessGame rules
   case find (\a -> (rule ^. rNumber) == (a ^. rNumber)) rs of
      Nothing -> do
         modifyGame rules (rule:)
         triggerEvent (RuleEv Added) rule
         return True
      Just _ -> return False


evModifyRule :: RuleNumber -> RuleInfo -> Evaluate Bool
evModifyRule rn rule = do
   (rs, _) <- accessGame rules
   let newRules = replaceWith ((== rn) . getL rNumber) rule rs
   case find ((== rn) . getL rNumber) rs of
      Nothing -> return False
      Just r ->  do
         putGame rules newRules
         triggerEvent (RuleEv Modified) r
         return True

evDelPlayer :: PlayerNumber -> Evaluate Bool
evDelPlayer pn = do
   g <- use eGame
   case find ((== pn) . getL playerNumber) (_players g) of
      Nothing -> do
         tracePN pn "not in game!"
         return False
      Just pi -> do
         modifyGame players $ filter (\a -> a ^. playerNumber /= pn)
         triggerEvent (Player Leave) pi
         tracePN pn $ "leaving the game: " ++ _gameName g
         return True

evChangeName :: PlayerNumber -> PlayerName -> Evaluate Bool
evChangeName pn name = do
   pls <- use (eGame . players)
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> return False
      Just pi -> do
         putGame players $ replaceWith ((== pn) . getL playerNumber) (pi {_playerName = name}) pls
         return True

evDelEvent :: EventNumber -> Evaluate Bool
evDelEvent en = do
   evs <- use (eGame . events)
   case find ((== en) . getL eventNumber) evs of
      Nothing -> return False
      Just eh -> case _evStatus eh of
         SActive -> do
            putGame events $ replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs
            return True
         SDeleted -> return False

evTriggerTime :: UTCTime -> Evaluate ()
evTriggerTime t = triggerEvent (Time t) t

evNewOutput :: Maybe PlayerNumber -> NomexNE String -> Evaluate OutputNumber
evNewOutput pn s = do
   (ops, rn) <- accessGame outputs
   let on = getFreeNumber (map _outputNumber ops)
   modifyGame outputs (Output on rn pn s SActive : )
   return on

evGetOutput :: OutputNumber -> EvaluateNE (Maybe String)
evGetOutput on = do
   ops <- _outputs <$> asks _eGame
   case find (\(Output myOn _ _ _ s) -> myOn == on && s == SActive) ops of
      Nothing -> return Nothing
      Just (Output _ _ _ o _) -> do
         out <- evalNomexNE o
         return $ Just out

evUpdateOutput :: OutputNumber -> NomexNE String -> Evaluate Bool
evUpdateOutput on s = do
   (ops, _) <- accessGame outputs
   case find (\(Output myOn _ _ _ s) -> myOn == on && s == SActive) ops of
      Nothing -> return False
      Just (Output _ rn pn _ _) -> do
         modifyGame outputs $ replaceWith ((== on) . getL outputNumber) (Output on rn pn s SActive)
         return True

evDelOutput :: OutputNumber -> Evaluate Bool
evDelOutput on = do
   ops <- use (eGame . outputs)
   case find ((== on) . getL outputNumber) ops of
      Nothing -> return False
      Just o -> case _oStatus o of
         SActive -> do
            putGame outputs $ replaceWith ((== on) . getL outputNumber) o{_oStatus = SDeleted} ops
            return True
         SDeleted -> return False

evSetVictory :: NomexNE [PlayerNumber] -> Evaluate ()
evSetVictory ps = do
   rn <- use eRuleNumber
   putGame victory (Just $ VictoryInfo rn ps)
   triggerEvent Victory (VictoryInfo rn ps)

evReadVar :: (Typeable a, Show a) => V a -> EvaluateNE (Maybe a)
evReadVar (V name) = do
   vars <- _variables <$> asks _eGame
   let var = find ((== name) . getL vName) vars
   case var of
      Nothing -> return Nothing
      Just (Var _ _ val) -> case cast val of
          Just v -> return $ Just v
          Nothing -> return Nothing

evGetRandomNumber :: Random a => (a, a) -> Evaluate a
evGetRandomNumber r = do
   g <- use (eGame . randomGen)
   let (a, g') = randomR r g
   putGame randomGen g'
   return a

--TODO should we also give a rule number to simulate the Nomex with?
-- currently we use the simulating rule number
evSimu :: Nomex a -> NomexNE Bool -> EvaluateNE Bool
evSimu sim ev = do
   rn <- asks _eRuleNumber
   let s = runEvalError rn Nothing (evalNomex sim)
   g <- asks _eGame
   let g' = execState s g
   return $ runEvaluateNE g' rn (evalNomexNE ev)

-- * misc

getVictorious :: Game -> [PlayerNumber]
getVictorious g = case _victory g of
   Nothing -> []
   Just (VictoryInfo rn v) -> runEvaluateNE g rn (evalNomexNE v)

evalOutput :: Game -> Output -> String
evalOutput g (Output _ rn _ o _) = runEvaluateNE g rn (evalNomexNE o)

allOutputs :: Game -> [String]
allOutputs g = map (evalOutput g) (_outputs g)

--delete all variables of a rule
delVarsRule :: RuleNumber -> Evaluate ()
delVarsRule rn = void $ (eGame . variables) %= filter ((/= rn) . getL vRuleNumber)

--delete all events of a rule
delEventsRule :: RuleNumber -> Evaluate ()
delEventsRule rn = do
   evs <- use (eGame . events)
   let toDelete = filter ((== rn) . getL ruleNumber) evs
   mapM_ (evDelEvent . _eventNumber) toDelete

--delete all outputs of a rule
delOutputsRule :: RuleNumber -> Evaluate ()
delOutputsRule rn = do
   os <- use (eGame . outputs)
   let toDelete = filter ((== rn) . getL oRuleNumber) os
   mapM_ (evDelOutput . _outputNumber) toDelete

--delete victory of a rule
delVictoryRule :: RuleNumber -> Evaluate ()
delVictoryRule rn = do
   vic <- use (eGame . victory)
   when (isJust vic && _vRuleNumber (fromJust vic) == rn) $ (eGame . victory) .= Nothing

--extract the game state from an Evaluate
--knowing the rule number performing the evaluation (0 if by the system)
--and the player number to whom display errors (set to Nothing for all players)
--TODO: clean
runEvalError :: RuleNumber -> (Maybe PlayerNumber) -> Evaluate a -> State Game ()
runEvalError rn mpn egs = modify (\g -> _eGame $ execState (runEvalError' mpn egs) (EvalEnv rn g evalNomex evalNomexNE))

runEvalError' :: (Maybe PlayerNumber) -> Evaluate a -> State EvalEnv ()
runEvalError' mpn egs = do
   e <- runErrorT egs
   case e of
      Right _ -> return ()
      Left e' -> do
         tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
         void $ runErrorT $ log mpn "Error: "

runSystemEval :: PlayerNumber -> Evaluate a -> State Game ()
runSystemEval pn e = runEvalError 0 (Just pn) e

runSystemEval' :: Evaluate a -> State Game ()
runSystemEval' e = runEvalError 0 Nothing e

--get the signals left to be completed in an event
getRemainingSignals :: EventInfo -> Game -> [(SignalAddress, SomeSignal)]
getRemainingSignals (EventInfo _ rn e _ _ env) g = case runEvaluateNE g rn (getEventResult e env) of
   Done _ -> []
   Todo a -> a

runEvaluateNE :: Game -> RuleNumber -> EvaluateNE a -> a
runEvaluateNE g rn ev = runReader ev (EvalEnv rn g evalNomex evalNomexNE)

runEvaluate :: Game -> RuleNumber -> State EvalEnv a -> a
runEvaluate g rn ev = evalState ev (EvalEnv rn g evalNomex evalNomexNE)

-- | Show instance for Game
-- showing a game involves evaluating some parts (such as victory and outputs)
instance Show Game where
   show g@(Game gn _ rs ps vs es os _ l t _) =
      "Game Name = "      ++ show gn ++
      "\n\n Rules = "       ++ (intercalate "\n " $ map show rs) ++
      "\n\n Players = "     ++ show ps ++
      "\n\n Variables = "   ++ show vs ++
      "\n\n Events = "      ++ (intercalate "\n " $ map (displayEvent g) es) ++ "\n" ++
      "\n\n Outputs = "     ++ (intercalate "\n " $ map (displayOutput g) os) ++ "\n" ++
      "\n\n Victory = "     ++ show (getVictorious g) ++
      "\n\n currentTime = " ++ show t ++ "\n" ++
      "\n\n logs = " ++ show l ++ "\n"


displayEvent :: Game -> EventInfo -> String
displayEvent g ei@(EventInfo en rn _ _ s env) =
   "event num: " ++ (show en) ++
   ", rule num: " ++ (show rn) ++
   ", remaining signals: " ++ (show $ getRemainingSignals ei g) ++ --TODO: display also event result?
   ", envs: " ++ (show env) ++
   ", status: " ++ (show s)

displayOutput :: Game -> Output -> String
displayOutput g o@(Output on rn mpn _ s) =
   "output num: " ++ (show on) ++
   ", rule num: " ++ (show rn) ++
   ", by pn: " ++ (show mpn) ++
   ", output: " ++ (show $ evalOutput g o) ++
   ", status: " ++ (show s)
