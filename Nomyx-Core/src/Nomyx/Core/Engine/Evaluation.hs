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
import Data.Todo
import Data.Either
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

--TODO should we also give a rule number to simulate the Nomex with?
-- currently we use the simulating rule number
evSimu :: Nomex a -> NomexNE Bool -> EvaluateNE Bool
evSimu sim ev = do
   rn <- asks _eRuleNumber
   let s = runEvalError rn Nothing (evalNomex sim)
   g <- asks _eGame
   let g' = execState s g
   return $ runReader (evalNomexNE ev) (EvalEnv rn g')

evNewVar :: (Typeable a, Show a) => VarName -> a -> Evaluate (Maybe (V a))
evNewVar name def = do
   (vars, rn) <- accessGame variables
   case find ((== name) . getL vName) vars of
      Nothing -> do
         (eGame >>> variables) %= (Var rn name def : )
         return $ Just (V name)
      Just _ -> return Nothing

evDelVar :: (V a) -> Evaluate Bool
evDelVar (V name) = do
   (vars, _) <- accessGame variables
   case find ((== name) . getL vName) vars of
      Nothing -> return False
      Just _ -> do
         (eGame >>> variables) %= filter ((/= name) . getL vName)
         return True

evWriteVar :: (Typeable a, Show a) => V a -> a -> Evaluate Bool
evWriteVar (V name) val = do
   (vars, _) <- accessGame variables
   case find (\(Var _ myName _) -> myName == name) vars of
      Nothing -> return False
      Just (Var rn myName _) -> do
         (eGame >>> variables) %= replaceWith ((== name) . getL vName) (Var rn myName val)
         return True

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> Evaluate EventNumber
evOnEvent event handler = do
   (evs, rn) <- accessGame events
   tracePN 0 (show rn)
   let en = getFreeNumber (map _eventNumber evs)
   (eGame >>> events) %= (EventInfo en rn event handler SActive [] : )
   return en

evSendMessage :: (Typeable a, Show a) => Msg a -> a -> Evaluate ()
evSendMessage (Msg id) d = triggerEvent (Message (Msg id)) d

evProposeRule :: RuleInfo -> Evaluate Bool
evProposeRule rule = do
   (rs, _) <- accessGame rules
   case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
      Nothing -> do
         (eGame >>> rules) %= (rule:)
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
         let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Active, _rAssessedBy = Just by} rs
         (eGame >>> rules) ~= newrules
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
         let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Reject, _rAssessedBy = Just by} rs
         (eGame >>> rules) ~= newrules
         triggerEvent (RuleEv Rejected) r
         delVarsRule rn
         delEventsRule rn
         delOutputsRule rn
         return True

evAddRule :: RuleInfo -> Evaluate Bool
evAddRule rule = do
   (rs, _) <- accessGame rules
   case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
      Nothing -> do
         (eGame >>> rules) %= (rule:)
         triggerEvent (RuleEv Added) rule
         return True
      Just _ -> return False


--TODO: clean and execute new rule
evModifyRule :: RuleNumber -> RuleInfo -> Evaluate Bool
evModifyRule mod rule = do
   (rs, rn) <- accessGame rules
   let newRules = replaceWith ((== rn) . getL rNumber) rule rs
   case find ((== rn) . getL rNumber) rs of
      Nothing -> return False
      Just r ->  do
         (eGame >>> rules) ~= newRules
         triggerEvent (RuleEv Modified) r
         return True

evDelPlayer :: PlayerNumber -> Evaluate Bool
evDelPlayer pn = do
   g <- access eGame
   case find ((== pn) . getL playerNumber) (_players g) of
      Nothing -> do
         tracePN pn "not in game!"
         return False
      Just pi -> do
         (eGame >>> players) %= filter ((/= pn) . getL playerNumber)
         triggerEvent (Player Leave) pi
         tracePN pn $ "leaving the game: " ++ _gameName g
         return True

evChangeName :: PlayerNumber -> PlayerName -> Evaluate Bool
evChangeName pn name = do
   pls <- access (eGame >>> players)
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> return False
      Just pi -> do
         (eGame >>> players) ~= replaceWith ((== pn) . getL playerNumber) (pi {_playerName = name}) pls
         return True

evDelEvent :: EventNumber -> Evaluate Bool
evDelEvent en = do
   evs <- access (eGame >>> events)
   case find ((== en) . getL eventNumber) evs of
      Nothing -> return False
      Just eh -> case _evStatus eh of
         SActive -> do
            let newEvents = replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs
            (eGame >>> events) ~= newEvents
            return True
         SDeleted -> return False

evTriggerTime :: UTCTime -> Evaluate ()
evTriggerTime t = triggerEvent (Time t) t

evNewOutput :: Maybe PlayerNumber -> NomexNE String -> Evaluate OutputNumber
evNewOutput pn s = do
   (ops, rn) <- accessGame outputs
   let on = getFreeNumber (map _outputNumber ops)
   (eGame >>> outputs) %= (Output on rn pn s SActive : )
   return on

evGetOutput :: OutputNumber -> EvaluateNE (Maybe String)
evGetOutput on = do
   ops <- _outputs <$> asks _eGame
   case find (\(Output myOn _ _ _ s) -> myOn == on && s == SActive) ops of
      Nothing -> return Nothing
      Just (Output _ rn _ o _) -> do
         out <- evalNomexNE o
         return $ Just out

evUpdateOutput :: OutputNumber -> NomexNE String -> Evaluate Bool
evUpdateOutput on s = do
   (ops, rn) <- accessGame outputs
   case find (\(Output myOn _ _ _ s) -> myOn == on && s == SActive) ops of
      Nothing -> return False
      Just (Output _ rn pn _ _) -> do
         (eGame >>> outputs) %= replaceWith ((== on) . getL outputNumber) (Output on rn pn s SActive)
         return True

evDelOutput :: OutputNumber -> Evaluate Bool
evDelOutput on = do
   ops <- access (eGame >>> outputs)
   case find ((== on) . getL outputNumber) ops of
      Nothing -> return False
      Just o -> case _oStatus o of
         SActive -> do
            let newOutputs = replaceWith ((== on) . getL outputNumber) o{_oStatus = SDeleted} ops
            (eGame >>> outputs) ~= newOutputs
            return True
         SDeleted -> return False

evSetVictory :: NomexNE [PlayerNumber] -> Evaluate ()
evSetVictory ps = do
   rn <- access eRuleNumber
   void $ (eGame >>> victory) ~= (Just $ VictoryInfo rn ps)
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

-- * Events
-- | Type agnostic result data
data SomeEvent = forall e. SomeEvent (Event e)
deriving instance Typeable SomeEvent


--get the fields left to be completed in an event
getEventFields :: EventInfo -> Game -> [(FieldAddress, SomeField)]
getEventFields ei@(EventInfo _ rn e _ _ er) g = getEventFields' e er g rn

--TODO: collapse with above
getEventFields' :: Event a -> [FieldResult] -> Game -> RuleNumber -> [(FieldAddress, SomeField)]
getEventFields' e er g rn = case (getEventResult e er g rn) of
   Done _ -> []
   Todo a -> a

-- | Get the field at a certain address
--TODO: should we check that the field is not already completed?
getInput :: EventInfo -> FieldAddress -> Game -> Maybe SomeField
getInput (EventInfo _ rn e _ _ er) addr g = findField addr e er g rn

findField :: FieldAddress -> Event e -> [FieldResult] -> Game -> RuleNumber -> Maybe SomeField
findField []         (BaseEvent f)    _   _ _ = Just $ SomeField f
findField (SumL:as)  (SumEvent e1 _)  frs g rn = findField as e1 (removePathElem SumL frs) g rn
findField (SumR:as)  (SumEvent _ e2)  frs g rn = findField as e2 (removePathElem SumR frs) g rn
findField (AppL:as)  (AppEvent e1 _)  frs g rn = findField as e1 (removePathElem AppL frs) g rn
findField (AppR:as)  (AppEvent _ e2)  frs g rn = findField as e2 (removePathElem AppR frs) g rn
findField (BindL:as) (BindEvent e1 _) frs g rn = findField as e1 (removePathElem BindL frs) g rn
findField (BindR:as) (BindEvent e1 f) frs g rn = case (getEventResult e1 (removePathElem BindL frs) g rn) of
   Done e2 -> findField as (f e2) (removePathElem BindR frs) g rn
   Todo _ -> Nothing

findField ((Index i):as) (ShortcutEvents es _) frs g rn = findField as (es!!i) frs g rn
findField _ _ _ _ _ = error "findField: wrong field address"

removePathElem :: FieldAddressElem -> [FieldResult] -> [FieldResult]
removePathElem fa frs = mapMaybe (removePathElem' fa) frs

removePathElem' :: FieldAddressElem -> FieldResult -> Maybe FieldResult
removePathElem' fa (FieldResult fe fr (Just (fa':fas))) = if (fa == fa')
   then Just $ FieldResult fe fr (Just fas)
   else Nothing

-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some fields results are pending, return that list instead.
getEventResult :: Event a -> [FieldResult] -> Game -> RuleNumber -> Todo (FieldAddress, SomeField) a
getEventResult e frs g rn = getEventResult' e frs [] g rn

getEventResult' :: Event a -> [FieldResult] -> FieldAddress -> Game -> RuleNumber -> Todo (FieldAddress, SomeField) a
getEventResult' (PureEvent a)  _   _  _ _ = Done a
getEventResult' EmptyEvent     _   _  _ _ = Todo []
getEventResult' (SumEvent a b) ers fa g rn = getEventResult' a ers (fa ++ [SumL]) g rn <|> getEventResult' b ers (fa ++ [SumR]) g rn
getEventResult' (AppEvent f b) ers fa g rn = getEventResult' f ers (fa ++ [AppL]) g rn <*> getEventResult' b ers (fa ++ [AppR]) g rn

getEventResult' (BindEvent a f) ers fa g rn = case (getEventResult' a ers (fa ++ [BindL]) g rn) of
   Done a' -> getEventResult' (f a') ers (fa ++ [BindR]) g rn
   Todo bs -> Todo bs

getEventResult' (BaseEvent a)  ers fa _ _ = case lookupField a fa ers of
   Just r  -> Done r
   Nothing -> Todo [(fa, SomeField a)]

getEventResult' (ShortcutEvents es f) ers fa g rn =
  let res = partitionEithers $ toEither <$> map (\i -> getEventResult' (es!!i) ers (fa ++ [Index i]) g rn) [0.. (length es -1)]
  in case f (snd res) of
       Just a  -> Done a
       Nothing -> Todo $ join $ fst res

getEventResult' (LiftNomexNE a) ers fa g rn = Done $ runReader (evalNomexNE a) (EvalEnv rn g)


-- trigger an event
triggerEvent :: (Typeable e, Show e) => Field e -> e -> Evaluate ()
triggerEvent e dat = do
   evs <- access (eGame >>> events)
   triggerEvent' (FieldResult e dat Nothing) evs

-- trigger some specific events
triggerEvent' :: FieldResult -> [EventInfo] -> Evaluate ()
triggerEvent' res evs = do
   evs' <- mapM (updateEventInfo res) evs
   (eGame >>> events) %= union (map fst evs')
   mapM triggerIfComplete evs'
   return ()

-- if the event is complete, trigger its handler
triggerIfComplete :: (EventInfo, Maybe SomeData) -> Evaluate ()
triggerIfComplete (EventInfo en rn _ h SActive _, Just (SomeData val)) = do
   case (cast val) of
      Just a -> do
         let exp = h (en, a)
         --tracePN 0 ("rn= " ++ (show rn) ++ "oldRn=" ++ (show oldRn) ++ "newRn=" ++ (show newRn))
         withRN rn $ (evalNomex exp) `catchError` (errorHandler en)
         return ()
      Nothing -> error "Bad trigger data type"
triggerIfComplete _ = return ()


-- update the EventInfo with the field result
updateEventInfo :: FieldResult -> EventInfo -> Evaluate (EventInfo, Maybe SomeData)
updateEventInfo (FieldResult field dat addr) ei@(EventInfo _ rn ev _ _ envi) = do
   g <- access eGame
   if (SomeField field) `elem` (map snd $ getEventFields' ev envi g rn)      -- if the field if found among the remaining fields of the event
      then case getEventResult ev (eventRes : envi) g rn of                 -- then check the event with that field result included
         Todo _ -> return (env ^=  (eventRes : envi) $ ei, Nothing)           -- some fields are left to complete: add ours in the environment
         Done a -> return (env ^=  []                $ ei, Just $ SomeData a) -- the event is now complete: empty the environment and output the result
      else         return (ei,                             Nothing)           -- field not found: do nothing
   where eventRes = FieldResult field dat addr


-- * input triggers

-- trigger the input event with the input data
triggerInput :: EventNumber -> FieldAddress -> InputData -> Evaluate ()
triggerInput en fa ir = do
   evs <- access (eGame >>> events)
   g <- access eGame
   let mei = find ((== en) . getL eventNumber) evs
   when (isJust mei) $ execInputHandler ir fa (fromJust mei) g

-- execute the corresponding handler
execInputHandler :: InputData -> FieldAddress -> EventInfo -> Game -> Evaluate ()
execInputHandler ir fa ei@(EventInfo _ _ _ _ SActive _) g = do
   case (getInput ei fa g) of
      Just sf -> execInputHandler' ir sf fa ei
      Nothing -> logAll "Input not found"
execInputHandler _ _ _ _ = return ()

-- execute the event handler using the data received from user
execInputHandler' :: InputData -> SomeField -> FieldAddress -> EventInfo -> Evaluate ()
execInputHandler' (TextData s)      (SomeField e@(Input _ _ (Text)))        fa ei = triggerEvent' (FieldResult e s                     (Just fa)) [ei]
execInputHandler' (TextAreaData s)  (SomeField e@(Input _ _ (TextArea)))    fa ei = triggerEvent' (FieldResult e s                     (Just fa)) [ei]
execInputHandler' (ButtonData)      (SomeField e@(Input _ _ (Button)))      fa ei = triggerEvent' (FieldResult e ()                    (Just fa)) [ei]
execInputHandler' (RadioData i)     (SomeField e@(Input _ _ (Radio cs)))    fa ei = triggerEvent' (FieldResult e (fst $ cs!!i)         (Just fa)) [ei]
execInputHandler' (CheckboxData is) (SomeField e@(Input _ _ (Checkbox cs))) fa ei = triggerEvent' (FieldResult e (fst <$> cs `sel` is) (Just fa)) [ei]
execInputHandler' _ _ _ _ = return ()

-- * misc

getVictorious :: Game -> [PlayerNumber]
getVictorious g = case _victory g of
   Nothing -> []
   Just (VictoryInfo rn v) -> runReader (evalNomexNE v) (EvalEnv rn g)

evalOutput :: Game -> Output -> String
evalOutput g (Output _ rn _ o _) = runReader (evalNomexNE o) (EvalEnv rn g)

allOutputs :: Game -> [String]
allOutputs g = map (evalOutput g) (_outputs g)

--delete all variables of a rule
delVarsRule :: RuleNumber -> Evaluate ()
delVarsRule rn = void $ (eGame >>> variables) %= filter ((/= rn) . getL vRuleNumber)

--delete all events of a rule
delEventsRule :: RuleNumber -> Evaluate ()
delEventsRule rn = do
   evs <- access (eGame >>> events)
   let toDelete = filter ((== rn) . getL ruleNumber) evs
   mapM_ (evDelEvent . _eventNumber) toDelete

--delete all outputs of a rule
delOutputsRule :: RuleNumber -> Evaluate ()
delOutputsRule rn = do
   os <- access (eGame >>> outputs)
   let toDelete = filter ((== rn) . getL oRuleNumber) os
   mapM_ (evDelOutput . _outputNumber) toDelete

-- | Show instance for Game
-- showing a game involves evaluating some parts (such as victory and outputs)
instance Show Game where
   show g@(Game gn _ rs ps vs es _ v _ t) =
      "Game Name = "      ++ show gn ++
      "\n Rules = "       ++ (intercalate "\n " $ map show rs) ++
      "\n Players = "     ++ show ps ++
      "\n Variables = "   ++ show vs ++
      "\n Events = "      ++ (intercalate "\n " $ map (displayEvent g) es) ++
      "\n Outputs = "     ++ show (allOutputs g) ++
      "\n Victory = "     ++ show (getVictorious g) ++
      "\n currentTime = " ++ show t ++ "\n"


displayEvent :: Game -> EventInfo -> String
displayEvent g ei@(EventInfo en rn e _ s env) =
   "event num: " ++ (show en) ++
   ", rule num: " ++ (show rn) ++
   ", event fields: " ++ (show $ getEventFields ei g) ++
   ", envs: " ++ (show env) ++
   ", status: " ++ (show s)


