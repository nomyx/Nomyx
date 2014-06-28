{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Evaluation of a Nomyx expression
module Nomyx.Core.Engine.Evaluation where

import Prelude hiding ((.), log)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Typeable
import Data.Time
import Data.Lens
import Data.Maybe
import Control.Category hiding (id)
import Control.Applicative
import Control.Monad.Error.Class (MonadError(..))
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.EvalUtils
import Nomyx.Core.Engine.Utils

-- * Evaluation

-- | evaluate an effecful expression.
-- The rule number passed is the number of the rule containing the expression.
evalNomex :: Nomex a -> RuleNumber -> Evaluate a
evalNomex (NewVar v a)            rn = evNewVar v a rn
evalNomex (DelVar v)              _  = evDelVar v
evalNomex (WriteVar v val)        _  = evWriteVar v val
evalNomex (OnEvent ev h)          rn = evOnEvent ev h rn
evalNomex (DelEvent en)           _  = evDelEvent en
evalNomex (SendMessage m d)       _  = evSendMessage m d
evalNomex (NewOutput pn s)        rn = evNewOutput pn rn s
evalNomex (UpdateOutput on s)     _  = evUpdateOutput on s
evalNomex (DelOutput on)          _  = evDelOutput on
evalNomex (ProposeRule rule)      _  = evProposeRule rule
evalNomex (ActivateRule rule)     rn = evActivateRule rule rn
evalNomex (RejectRule rule)       rn = evRejectRule rule rn
evalNomex (AddRule rule)          _  = evAddRule rule
evalNomex (ModifyRule mod rule)   _  = evModifyRule mod rule
evalNomex (SetPlayerName pn n)    _  = evChangeName pn n
evalNomex (DelPlayer pn)          _  = evDelPlayer pn
evalNomex (SetVictory ps)         rn = evSetVictory ps rn
evalNomex (LiftEffect e)          pn = liftEval $ evalNomexNE e pn
evalNomex (ThrowError s)          _  = throwError s
evalNomex (CatchError n h)        rn = catchError (evalNomex n rn) (\a -> evalNomex (h a) rn)
evalNomex (Return a)              _  = return a
evalNomex (Bind exp f)            rn = evalNomex exp rn >>= \e -> evalNomex (f e) rn

-- | evaluate an effectless expression.
evalNomexNE :: NomexNE a -> RuleNumber -> Reader Game a
evalNomexNE (ReadVar v)     _  = evReadVar v
evalNomexNE (GetOutput on)  _  = evGetOutput on
evalNomexNE  GetRules       _  = asks _rules
evalNomexNE  GetPlayers     _  = asks _players
evalNomexNE  GetEvents      _  = asks _events
evalNomexNE  SelfRuleNumber rn = return rn
evalNomexNE (CurrentTime)   _  = asks _currentTime
evalNomexNE (Return a)      _  = return a
evalNomexNE (Bind exp f)    rn = evalNomexNE exp rn >>= \e -> evalNomexNE (f e) rn
evalNomexNE (Simu sim ev)   rn = evSimu sim ev rn

evSimu :: Nomex a -> NomexNE Bool -> RuleNumber -> Reader Game Bool
evSimu sim ev rn = do
   let s = runEvalError Nothing (evalNomex sim rn)
   g <- ask
   let g' = execState s g
   return $ runReader (evalNomexNE ev rn) g'

evNewVar :: (Typeable a, Show a) => VarName -> a -> RuleNumber -> Evaluate (Maybe (V a))
evNewVar name def rn = do
   vars <- access variables
   case find ((== name) . getL vName) vars of
      Nothing -> do
         variables %= (Var rn name def : )
         return $ Just (V name)
      Just _ -> return Nothing

evDelVar :: (V a) -> Evaluate Bool
evDelVar (V name) = do
   vars <- access variables
   case find ((== name) . getL vName) vars of
      Nothing -> return False
      Just _ -> do
         variables %= filter ((/= name) . getL vName)
         return True

evWriteVar :: (Typeable a, Show a) => V a -> a -> Evaluate Bool
evWriteVar (V name) val  = do
   vars <- access variables
   case find (\(Var _ myName _) -> myName == name) vars of
      Nothing -> return False
      Just (Var rn myName _) -> do
         variables %= replaceWith ((== name) . getL vName) (Var rn myName val)
         return True

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> RuleNumber -> Evaluate EventNumber
evOnEvent event handler rn = do
   evs <- access events
   let en = getFreeNumber (map _eventNumber evs)
   events %= (EventInfo en rn (indexInputs event) handler SActive [] : )
   return en

evSendMessage :: (Typeable a, Show a) => Msg a -> a -> Evaluate ()
evSendMessage (Msg id) d = triggerEvent (Message (Msg id)) d

evProposeRule :: RuleInfo -> Evaluate Bool
evProposeRule rule = do
   rs <- access rules
   case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
      Nothing -> do
         rules %= (rule:)
         triggerEvent (RuleEv Proposed) rule
         return True
      Just _ -> return False

--Sets the rule status to Active and execute it if possible
evActivateRule :: RuleNumber -> RuleNumber -> Evaluate Bool
evActivateRule rn by = do
   rs <- access rules
   case find (\r -> _rNumber r == rn && _rStatus r /= Active) rs of
      Nothing -> return False
      Just r -> do
         let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Active, _rAssessedBy = Just by} rs
         rules ~= newrules
         --execute the rule
         evalNomex (_rRule r) rn
         triggerEvent (RuleEv Activated) r
         return True

evRejectRule :: RuleNumber -> RuleNumber -> Evaluate Bool
evRejectRule rn by = do
   rs <- access rules
   case find (\r -> _rNumber r == rn && _rStatus r /= Reject) rs of
      Nothing -> return False
      Just r -> do
         let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Reject, _rAssessedBy = Just by} rs
         rules ~= newrules
         triggerEvent (RuleEv Rejected) r
         delVarsRule rn
         delEventsRule rn
         delOutputsRule rn
         return True

evAddRule :: RuleInfo -> Evaluate Bool
evAddRule rule = do
   rs <- access rules
   case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
      Nothing -> do
         rules %= (rule:)
         triggerEvent (RuleEv Added) rule
         return True
      Just _ -> return False


--TODO: clean and execute new rule
evModifyRule :: RuleNumber -> RuleInfo -> Evaluate Bool
evModifyRule mod rule = do
   rs <- access rules
   let newRules = replaceWith ((== mod) . getL rNumber) rule rs
   case find ((== mod) . getL rNumber) rs of
      Nothing -> return False
      Just r ->  do
         rules ~= newRules
         triggerEvent (RuleEv Modified) r
         return True

evDelPlayer :: PlayerNumber -> Evaluate Bool
evDelPlayer pn = do
   g <- get
   case find ((== pn) . getL playerNumber) (_players g) of
      Nothing -> do
         tracePN pn "not in game!"
         return False
      Just pi -> do
         players %= filter ((/= pn) . getL playerNumber)
         triggerEvent (Player Leave) pi
         tracePN pn $ "leaving the game: " ++ _gameName g
         return True

evChangeName :: PlayerNumber -> PlayerName -> Evaluate Bool
evChangeName pn name = do
   pls <- access players
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> return False
      Just pi -> do
         players ~= replaceWith ((== pn) . getL playerNumber) (pi {_playerName = name}) pls
         return True

evDelEvent :: EventNumber -> Evaluate Bool
evDelEvent en = do
   evs <- access events
   case find ((== en) . getL eventNumber) evs of
      Nothing -> return False
      Just eh -> case _evStatus eh of
         SActive -> do
            let newEvents = replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs
            events ~= newEvents
            return True
         SDeleted -> return False

evTriggerTime :: UTCTime -> Evaluate ()
evTriggerTime t = triggerEvent (Time t) t

evNewOutput :: Maybe PlayerNumber -> RuleNumber -> NomexNE String -> Evaluate OutputNumber
evNewOutput pn rn s = do
   ops <- access outputs
   let on = getFreeNumber (map _outputNumber ops)
   outputs %= (Output on rn pn s SActive : )
   return on

evGetOutput :: OutputNumber -> Reader Game (Maybe String)
evGetOutput on = do
   ops <- asks _outputs
   case find (\(Output myOn _ _ _ s) -> myOn == on && s == SActive) ops of
      Nothing -> return Nothing
      Just (Output _ rn _ o _) -> do
         out <- evalNomexNE o rn
         return $ Just out

evUpdateOutput :: OutputNumber -> NomexNE String -> Evaluate Bool
evUpdateOutput on s = do
   ops <- access outputs
   case find (\(Output myOn _ _ _ s) -> myOn == on && s == SActive) ops of
      Nothing -> return False
      Just (Output _ rn pn _ _) -> do
         outputs %= replaceWith ((== on) . getL outputNumber) (Output on rn pn s SActive)
         return True

evDelOutput :: OutputNumber -> Evaluate Bool
evDelOutput on = do
   ops <- access outputs
   case find ((== on) . getL outputNumber) ops of
      Nothing -> return False
      Just o -> case _oStatus o of
         SActive -> do
            let newOutputs = replaceWith ((== on) . getL outputNumber) o{_oStatus = SDeleted} ops
            outputs ~= newOutputs
            return True
         SDeleted -> return False

evSetVictory :: NomexNE [PlayerNumber] -> RuleNumber -> Evaluate ()
evSetVictory ps rn = do
   void $ victory ~= (Just $ VictoryCond rn ps)
   triggerEvent Victory (VictoryCond rn ps)

evReadVar :: (Typeable a, Show a) => V a -> Reader Game (Maybe a)
evReadVar (V name) = do
   vars <- asks _variables
   let var = find ((== name) . getL vName) vars
   case var of
      Nothing -> return Nothing
      Just (Var _ _ val) -> case cast val of
          Just v -> return $ Just v
          Nothing -> return Nothing

-- * Event triggers

-- trigger an event
triggerEvent :: (Typeable e, Show e) => Field e -> e -> Evaluate ()
triggerEvent e dat = do
   evs <- access events
   triggerEvent' e dat evs

-- trigger some specific events
triggerEvent' :: (Typeable e, Show e) => Field e -> e -> [EventInfo] -> Evaluate ()
triggerEvent' e dat evs = do
   let evs' = map (updateEventInfo e dat) evs
   events %= union (map fst evs')
   mapM triggerIfComplete evs'
   return ()

-- if the event is complete, trigger its handler
triggerIfComplete :: (EventInfo, Maybe SomeData) -> Evaluate ()
triggerIfComplete (EventInfo en rn _ h SActive _, Just (SomeData val)) = do
   case (cast val) of
      Just a -> do
         let exp = h (en, a)
         (evalNomex exp rn) `catchError` (errorHandler rn en)
      Nothing -> error "Bad trigger data type"
triggerIfComplete _ = return ()


-- * input triggers

-- trigger the input event with the input data
triggerInput :: EventNumber -> InputNumber -> InputData -> Evaluate ()
triggerInput en inn ir = do
   evs <- access events
   let mei = find ((== en) . getL eventNumber) evs
   when (isJust mei) $ execInputHandler ir inn (fromJust mei)

-- execute the corresponding handler
execInputHandler :: InputData -> InputNumber -> EventInfo -> Evaluate ()
execInputHandler ir inn ei@(EventInfo _ _ _ _ SActive _) = do
   case (getInput ei inn) of
      Just sf -> execInputHandler' ir sf ei
      Nothing -> logAll "Input not found"
execInputHandler _ _ _ = return ()

-- execute the event handler using the data received from user
execInputHandler' :: InputData -> SomeField -> EventInfo -> Evaluate ()
execInputHandler' (TextData s)      (SomeField e@(Input _ _ _ (Text)))        ei = triggerEvent' e s [ei]
execInputHandler' (TextAreaData s)  (SomeField e@(Input _ _ _ (TextArea)))    ei = triggerEvent' e s [ei]
execInputHandler' (ButtonData)      (SomeField e@(Input _ _ _ (Button)))      ei = triggerEvent' e () [ei]
execInputHandler' (RadioData i)     (SomeField e@(Input _ _ _ (Radio cs)))    ei = triggerEvent' e (fst $ cs!!i) [ei]
execInputHandler' (CheckboxData is) (SomeField e@(Input _ _ _ (Checkbox cs))) ei = triggerEvent' e (fst <$> cs `sel` is) [ei]
execInputHandler' _ _ _ = return ()

-- * misc

getVictorious :: Game -> [PlayerNumber]
getVictorious g = case _victory g of
   Nothing -> []
   Just (VictoryCond rn v) -> runReader (evalNomexNE v rn) g

evalOutput :: Game -> Output -> String
evalOutput g (Output _ rn _ o _) = runReader (evalNomexNE o rn) g

allOutputs :: Game -> [String]
allOutputs g = map (evalOutput g) (_outputs g)

--delete all variables of a rule
delVarsRule :: RuleNumber -> Evaluate ()
delVarsRule rn = void $ variables %= filter ((/= rn) . getL vRuleNumber)

--delete all events of a rule
delEventsRule :: RuleNumber -> Evaluate ()
delEventsRule rn = do
   evs <- access events
   let toDelete = filter ((== rn) . getL ruleNumber) evs
   mapM_ (evDelEvent . _eventNumber) toDelete

--delete all outputs of a rule
delOutputsRule :: RuleNumber -> Evaluate ()
delOutputsRule rn = do
   os <- access outputs
   let toDelete = filter ((== rn) . getL oRuleNumber) os
   mapM_ (evDelOutput . _outputNumber) toDelete


-- | Show instance for Game
-- showing a game involves evaluating some parts (such as victory and outputs)
instance Show Game where
   show g@(Game { _gameName, _rules, _players, _variables, _events, _victory, _currentTime}) =
      "Game Name = "      ++ show _gameName ++
      "\n Rules = "       ++ (intercalate "\n " $ map show _rules) ++
      "\n Players = "     ++ show _players ++
      "\n Variables = "   ++ show _variables ++
      "\n Events = "      ++ (intercalate "\n " $ map show _events) ++
      "\n Outputs = "     ++ show (allOutputs g) ++
      "\n Victory = "     ++ show (getVictorious g) ++
      "\n currentTime = " ++ show _currentTime ++ "\n"


