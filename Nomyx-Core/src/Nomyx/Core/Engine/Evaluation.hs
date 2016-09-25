{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Evaluation of a Nomyx expression
module Nomyx.Core.Engine.Evaluation where

import           Control.Applicative
import           Control.Category            hiding (id)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Todo
import           Data.Typeable
import           Imprevu.Evaluation.EventEval hiding (events)
import           Imprevu.Event
import           Language.Nomyx
import           Nomyx.Core.Engine.EvalUtils
import           Nomyx.Core.Engine.EventEval
import           Nomyx.Core.Engine.Types     hiding (_vRuleNumber)
import           Nomyx.Core.Engine.Utils
import           Prelude                     hiding (log, (.))
import           System.Random


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
evalNomex (ModifyRule m rule)     = evModifyRule m rule
evalNomex (SetPlayerName pn n)    = evChangeName pn n
evalNomex (DelPlayer pn)          = evDelPlayer pn
evalNomex (SetVictory ps)         = evSetVictory ps
--evalNomex (LiftEffect e)          = liftEval $ evalNomex e
evalNomex (ThrowError s)          = throwError s
evalNomex (CatchError n h)        = catchError (evalNomex n) (evalNomex . h)
evalNomex (Return a)              = return a
evalNomex (Bind ex f)             = evalNomex ex >>= \e -> evalNomex (f e)
evalNomex (GetRandomNumber r)     = evGetRandomNumber r

-- | evaluate an effectless expression.
evalNomex (ReadVar v)     = evReadVar v
evalNomex (GetOutput on)  = evGetOutput on
evalNomex  GetRules       = use (evalEnv . eGame . rules)
evalNomex  GetPlayers     = use (evalEnv . eGame . players)
evalNomex  GetEvents      = use (evalEnv . eGame . events) >>= return . (map _erEventInfo)
evalNomex  SelfRuleNumber = use (evalEnv . eRuleNumber)
evalNomex (GetCurrentTime)= use (evalEnv . eGame . currentTime)
evalNomex (Return a)      = return a
evalNomex (Bind ex f)     = evalNomex ex >>= \e -> evalNomex (f e)
evalNomex (Simu sim ev)   = evSimu sim ev


evNewVar :: (Typeable a, Show a) => VarName -> a -> Evaluate (Maybe (V a))
evNewVar name def = do
   (vars, rn) <- accessGame variables
   case find ((== name) . getL vName) vars of
      Nothing -> do
         modifyGame variables (Var rn name def : )
         return $ Just (V name)
      Just _ -> return Nothing

evDelVar :: V a -> Evaluate Bool
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
evOnEvent ev h = do
   (evs, rn) <- accessGame events
   let en = getFreeNumber (map (_eventNumber . _erEventInfo) evs)
   modifyGame events (\eis -> (RuleEventInfo rn (EventInfo en ev h SActive [])) : eis)
   return en

evSendMessage :: (Typeable a, Show a) => Msg a -> a -> Evaluate ()
evSendMessage m = triggerEvent (Signal m)

evProposeRule :: RuleInfo -> Evaluate Bool
evProposeRule rule = do
   (rs, _) <- accessGame rules
   case find (\a -> (rule ^. rNumber) == (a ^. rNumber)) rs of
      Nothing -> do
         modifyGame rules (rule:)
         triggerEvent (Signal Proposed) rule
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
         triggerEvent (Signal Activated) r
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
         triggerEvent (Signal Rejected) r
         return True

evAddRule :: RuleInfo -> Evaluate Bool
evAddRule rule = do
   (rs, _) <- accessGame rules
   case find (\a -> (rule ^. rNumber) == (a ^. rNumber)) rs of
      Nothing -> do
         modifyGame rules (rule:)
         triggerEvent (Signal Added) rule
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
         triggerEvent (Signal Modified) r
         return True

evDelPlayer :: PlayerNumber -> Evaluate Bool
evDelPlayer pn = do
   g <- use (evalEnv . eGame)
   case find ((== pn) . getL playerNumber) (_players g) of
      Nothing -> do
         tracePN pn "not in game!"
         return False
      Just pid -> do
         modifyGame players $ filter (\a -> a ^. playerNumber /= pn)
         triggerEvent (Signal Leave) pid
         tracePN pn $ "leaving the game: " ++ _gameName g
         return True

evChangeName :: PlayerNumber -> PlayerName -> Evaluate Bool
evChangeName pn name = do
   pls <- use (evalEnv . eGame . players)
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> return False
      Just pid -> do
         putGame players $ replaceWith ((== pn) . getL playerNumber) (pid {_playerName = name}) pls
         return True

evDelEvent :: EventNumber -> Evaluate Bool
evDelEvent en = do
   evs <- use (evalEnv . eGame . events)
   case find ((== en) . getL (erEventInfo . eventNumber)) evs of
      Nothing -> return False
      Just eh -> case (_evStatus $ _erEventInfo eh) of
         SActive -> do
            let evs' = replaceWith ((== en) . getL (erEventInfo . eventNumber))  (erEventInfo . evStatus .~ SDeleted $ eh) evs
            putGame events evs'
            return True
         SDeleted -> return False

evTriggerTime :: UTCTime -> Evaluate ()
evTriggerTime t = triggerEvent (Signal t) t

evNewOutput :: Maybe PlayerNumber -> Nomex String -> Evaluate OutputNumber
evNewOutput pn s = do
   (ops, rn) <- accessGame outputs
   let on = getFreeNumber (map _outputNumber ops)
   modifyGame outputs (Output on rn pn s SActive : )
   return on

evGetOutput :: OutputNumber -> Evaluate (Maybe String)
evGetOutput on = do
   ops <- use (evalEnv . eGame . outputs)
   case find (\(Output myOn _ _ _ s) -> myOn == on && s == SActive) ops of
      Nothing -> return Nothing
      Just (Output _ _ _ o _) -> do
         out <- evalNomex o
         return $ Just out

evUpdateOutput :: OutputNumber -> Nomex String -> Evaluate Bool
evUpdateOutput on s = do
   (ops, _) <- accessGame outputs
   case find (\(Output myOn _ _ _ st) -> myOn == on && st == SActive) ops of
      Nothing -> return False
      Just (Output _ rn pn _ _) -> do
         modifyGame outputs $ replaceWith ((== on) . getL outputNumber) (Output on rn pn s SActive)
         return True

evDelOutput :: OutputNumber -> Evaluate Bool
evDelOutput on = do
   ops <- use (evalEnv . eGame . outputs)
   case find ((== on) . getL outputNumber) ops of
      Nothing -> return False
      Just o -> case _oStatus o of
         SActive -> do
            putGame outputs $ replaceWith ((== on) . getL outputNumber) o{_oStatus = SDeleted} ops
            return True
         SDeleted -> return False

evSetVictory :: Nomex [PlayerNumber] -> Evaluate ()
evSetVictory ps = do
   rn <- use (evalEnv . eRuleNumber)
   putGame undefined (Just $ VictoryInfo rn ps)
   triggerEvent (Signal Victory) (VictoryInfo rn ps)

evReadVar :: (Typeable a, Show a) => V a -> Evaluate (Maybe a)
evReadVar (V name) = do
   vars <- use (evalEnv . eGame . variables)
   let var = find ((== name) . getL vName) vars
   case var of
      Nothing -> return Nothing
      Just (Var _ _ val) -> case cast val of
          Just v -> return $ Just v
          Nothing -> return Nothing

evGetRandomNumber :: Random a => (a, a) -> Evaluate a
evGetRandomNumber r = do
   g <- use (evalEnv . eGame . randomGen)
   let (a, g') = randomR r g
   putGame randomGen g'
   return a

--TODO should we also give a rule number to simulate the Nomex with?
-- currently we use the simulating rule number
evSimu :: Nomex a -> Nomex Bool -> Evaluate Bool
evSimu sim ev = do
   rn <- use (evalEnv . eRuleNumber)
   let s = runEvalError rn Nothing (evalNomex sim)
   g <- use (evalEnv . eGame)
   let g' = execState s g
   return $ runEvaluate' g' rn (evalNomex ev)

-- * misc

getVictorious :: Game -> [PlayerNumber]
getVictorious g = case _victory g of
   Nothing -> []
   Just (VictoryInfo rn v) -> runEvaluate' g rn (evalNomex v)

evalOutput :: Game -> Output -> String
evalOutput g (Output _ rn _ o _) = runEvaluate' g rn (evalNomex o)

allOutputs :: Game -> [String]
allOutputs g = map (evalOutput g) (_outputs g)

--delete all variables of a rule
delVarsRule :: RuleNumber -> Evaluate ()
delVarsRule rn = void $ (evalEnv . eGame . variables) %= filter ((/= rn) . getL vRuleNumber)

--delete all events of a rule
delEventsRule :: RuleNumber -> Evaluate ()
delEventsRule rn = do
   evs <- use (evalEnv . eGame . events)
   let toDelete = filter ((== rn) . getL (erRuleNumber)) evs
   mapM_ (evDelEvent . _eventNumber . _erEventInfo) toDelete

--delete all outputs of a rule
delOutputsRule :: RuleNumber -> Evaluate ()
delOutputsRule rn = do
   os <- use (evalEnv . eGame . outputs)
   let toDelete = filter ((== rn) . getL oRuleNumber) os
   mapM_ (evDelOutput . _outputNumber) toDelete

--delete victory of a rule
delVictoryRule :: RuleNumber -> Evaluate ()
delVictoryRule rn = do
   vic <- use (evalEnv . eGame . victory)
   when (isJust vic && _vRuleNumber (fromJust vic) == rn) $ (evalEnv . eGame . victory) .= Nothing

--extract the game state from an Evaluate
--knowing the rule number performing the evaluation (0 if by the system)
--and the player number to whom display errors (set to Nothing for all players)
--TODO: clean
runEvalError :: RuleNumber -> Maybe PlayerNumber -> Evaluate a -> State Game ()
runEvalError rn mpn eva = undefined
--modify (\g -> _eGame $ execState (runEvalError' mpn egs) (EvalEnv rn g evalNomex))

--runEvalError' :: EvaluateN n s a -> State (EvalEnvN n s) (Maybe a)

--runEvalError' :: Maybe PlayerNumber -> Evaluate a -> State EvalEnv ()
--runEvalError' mpn egs = do
--   e <- runErrorT egs
--   case e of
--      Right _ -> return ()
--      Left e' -> do
--         tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
--         void $ runErrorT $ log mpn "Error: "

runSystemEval :: PlayerNumber -> Evaluate a -> State Game ()
runSystemEval pn = runEvalError 0 (Just pn)

runSystemEval' :: Evaluate a -> State Game ()
runSystemEval' = runEvalError 0 Nothing

--get the signals left to be completed in an event
--getRemainingSignals :: EventInfo -> Game -> [(SignalAddress, SomeSignal)]
--getRemainingSignals (EventInfo _ rn e _ _ en) g = case runEvaluate g rn (getEventResult e en) of
--   Done _ -> []
--   Todo a -> a

--getRemainingSignals :: EventInfo -> Game -> [(SignalAddress, SomeSignal)]
--getRemainingSignals ei g = evalState (runEvalError'' Nothing (getRemainingSignals' ei)) (EvalEnv 0 g evalNomex)

--runEvalError'' :: Maybe PlayerNumber -> Evaluate a -> State EvalEnv a
--runEvalError'' mpn egs = do
--   e <- runErrorT egs
--   case e of
--      Right a -> return a
--      Left e' -> do
--         tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
--         void $ runErrorT $ log mpn "Error: "
--         error "Eval"
--runEvaluateNE :: Game -> RuleNumber -> Evaluate a -> a
--runEvaluateNE g rn ev = runReader ev (EvalEnv rn g evalNomex)

--runEvaluate :: Game -> RuleNumber -> State EvalEnv a -> a
--runEvaluate g rn ev = evalState ev (EvalEnv rn g evalNomex)

runEvaluate' :: Game -> RuleNumber -> Evaluate a -> a
runEvaluate' g rn ev = undefined --evalState (runEvalError'' Nothing ev) (EvalEnv rn g evalNomex)

-- | Show instance for Game
-- showing a game involves evaluating some parts (such as victory and outputs)
instance Show Game where
   show g@(Game gn _ rs ps vs es os _ l t _) =
      "Game Name = "      ++ show gn ++
      "\n\n Rules = "       ++ (intercalate "\n " $ map show rs) ++
      "\n\n Players = "     ++ show ps ++
      "\n\n Variables = "   ++ show vs ++
  --    "\n\n Events = "      ++ (intercalate "\n " $ map (displayEvent g) es) ++ "\n" ++
      "\n\n Outputs = "     ++ (intercalate "\n " $ map (displayOutput g) os) ++ "\n" ++
      "\n\n Victory = "     ++ show (getVictorious g) ++
      "\n\n currentTime = " ++ show t ++ "\n" ++
      "\n\n logs = " ++ show l ++ "\n"

deriving instance Show LoggedGame

displayOutput :: Game -> Output -> String
displayOutput g o@(Output on rn mpn _ s) =
   "output num: " ++ (show on) ++
   ", rule num: " ++ (show rn) ++
   ", by pn: " ++ (show mpn) ++
   ", output: " ++ (show $ evalOutput g o) ++
   ", status: " ++ (show s)
