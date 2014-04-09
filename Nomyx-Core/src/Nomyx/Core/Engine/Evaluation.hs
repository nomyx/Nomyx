{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nomyx.Core.Engine.Evaluation where

import Prelude hiding ((.), log)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Typeable
import Data.Function hiding ((.))
import Data.Time
import Data.Lens
import Data.Maybe
import Control.Category
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Applicative ((<$>))
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Game
import Nomyx.Core.Engine.Utils


type Evaluate a = ErrorT String (State Game) a

-- an untyped version of InputData for serialization
data UInputData = URadioData Int
                | UCheckboxData [Int]
                | UTextData String
                | UTextAreaData String
                | UButtonData
                  deriving (Show, Read, Eq, Ord)

-- | evaluate an expression.
-- The rule number passed is the number of the rule containing the expression.
evalNomex :: Nomex a -> RuleNumber -> Evaluate a
evalNomex (NewVar name def) rn = do
   vars <- access variables
   case find ((== name) . getL vName) vars of
      Nothing -> do
         variables %= (Var rn name def : )
         return $ Just (V name)
      Just _ -> return Nothing

evalNomex (DelVar (V name)) _ = do
   vars <- access variables
   case find ((== name) . getL vName) vars of
      Nothing -> return False
      Just _ -> do
         variables %= filter ((/= name) . getL vName)
         return True

evalNomex (WriteVar (V name) val) _ = do
   vars <- access variables
   case find (\(Var _ myName _) -> myName == name) vars of
      Nothing -> return False
      Just (Var rn myName _) -> do
         variables %= replaceWith ((== name) . getL vName) (Var rn myName val)
         return True

evalNomex (OnEvent event handler) rn = do
   evs <- access events
   let en = getFreeNumber (map _eventNumber evs)
   events %= (EH en rn event handler SActive : )
   return en

evalNomex (DelEvent en) _ = evDelEvent en

evalNomex (DelAllEvents e) _ = do
   evs <- access events
   let filtered = filter (\EH {event} -> event === e) evs
   mapM_ evDelEvent (_eventNumber <$> filtered)

evalNomex (SendMessage (Message id) myData) _ = triggerEvent_ (Message id) (MessageData myData)

evalNomex (NewOutput pn s)      rn = evNewOutput pn rn s
evalNomex (UpdateOutput on s)   _  = evUpdateOutput on s
evalNomex (DelOutput on)        _  = evDelOutput on
evalNomex (ProposeRule rule)    _  = evProposeRule rule
evalNomex (ActivateRule rule)   rn = evActivateRule rule rn
evalNomex (RejectRule rule)     rn = evRejectRule rule rn
evalNomex (AddRule rule)        _  = evAddRule rule
evalNomex (ModifyRule mod rule) _  = evModifyRule mod rule
evalNomex (SetPlayerName pn n)  _  = evChangeName pn n
evalNomex (DelPlayer pn)        _  = evDelPlayer pn
evalNomex (LiftEffect e)        pn = liftEval $ evalNomexNE e pn


evalNomex (ThrowError s)        _  = throwError s
evalNomex (CatchError n h)      rn = catchError (evalNomex n rn) (\a -> evalNomex (h a) rn)
evalNomex (SetVictory ps)       rn = do
   void $ victory ~= (Just $ VictoryCond rn ps)
   triggerEvent_ Victory (VictoryData $ VictoryCond rn ps)

evalNomex (Return a)            _  = return a
evalNomex (Bind exp f) rn = do
   e <- evalNomex exp rn
   evalNomex (f e) rn

liftEval :: Reader Game a -> Evaluate a
liftEval r = runReader r <$> get

evalNomexNE :: NomexNE a -> RuleNumber -> Reader Game a
evalNomexNE (ReadVar (V name)) _ = do
   vars <- asks _variables
   let var = find ((== name) . getL vName) vars
   case var of
      Nothing -> return Nothing
      Just (Var _ _ val) -> case cast val of
          Just v -> return $ Just v
          Nothing -> return Nothing

evalNomexNE (GetOutput on)        _  = evGetOutput on
evalNomexNE GetRules              _  = asks _rules
evalNomexNE GetPlayers            _  = asks _players
evalNomexNE SelfRuleNumber        rn = return rn
evalNomexNE (CurrentTime)         _  = asks _currentTime
evalNomexNE (Return a)            _  = return a
evalNomexNE (Bind exp f) rn = do
   e <- evalNomexNE exp rn
   evalNomexNE (f e) rn

evalNomexNE (Simu sim ev) rn = do
   let s = runEvalError Nothing (evalNomex sim rn)
   g <- ask
   let g' = execState s g
   return $ runReader (evalNomexNE ev rn) g'

getVictorious :: Game -> [PlayerNumber]
getVictorious g = case _victory g of
   Nothing -> []
   Just (VictoryCond rn v) -> runReader (evalNomexNE v rn) g

evalOutput :: Game -> Output -> String
evalOutput g (Output _ rn _ o _) = runReader (evalNomexNE o rn) g

allOutputs :: Game -> [String]
allOutputs g = map (evalOutput g) (_outputs g)

isOutput :: String -> Game -> Bool
isOutput s g = s `elem` allOutputs g

--execute all the handlers of the specified event with the given data
triggerEvent :: (Typeable e) => Event e -> EventData e -> Evaluate Bool
triggerEvent e dat = do
   evs <- access events
   let filtered = filter (\(EH {event, _evStatus}) -> e === event && _evStatus == SActive) (reverse evs)
   case filtered of
      [] -> return False
      xs -> do
         mapM_ (triggerHandler dat) xs
         return True


triggerHandler :: (Typeable e) => EventData e -> EventHandler -> Evaluate ()
triggerHandler dat (EH {_ruleNumber, _eventNumber, handler}) = case cast handler of
    Just castedH -> do
       let (exp :: Nomex ()) = castedH (_eventNumber, dat)
       (evalNomex exp _ruleNumber) `catchError` (errorHandler _ruleNumber _eventNumber)
    Nothing -> logAll ("failed " ++ (show $ typeOf handler))

triggerEvent_ :: (Typeable e) => Event e -> EventData e -> Evaluate ()
triggerEvent_ e ed = void $ triggerEvent e ed

errorHandler :: RuleNumber -> EventNumber -> String -> Evaluate ()
errorHandler rn en s = logAll $ "Error in rule " ++ show rn ++ " (triggered by event " ++ show en ++ "): " ++ s

-- trigger the input event with the input data
triggerInput :: EventNumber -> UInputData -> Evaluate ()
triggerInput en ir = do
   evs <- access events
   let filtered = filter ((== en) . getL eventNumber) evs
   mapM_ (execInputHandler ir) filtered


-- execute the event handler using the data received from user
execInputHandler :: UInputData -> EventHandler -> Evaluate ()
execInputHandler (UTextData s)      (EH en rn (InputEv (Input _ _ Text))          h SActive) = evalNomex (h (en, InputData $ TextData s)) rn
execInputHandler (UTextAreaData s)  (EH en rn (InputEv (Input _ _ TextArea))      h SActive) = evalNomex (h (en, InputData $ TextAreaData s)) rn
execInputHandler (UButtonData)      (EH en rn (InputEv (Input _ _ Button))        h SActive) = evalNomex (h (en, InputData $ ButtonData)) rn
execInputHandler (URadioData i)     (EH en rn (InputEv (Input _ _ (Radio cs)))    h SActive) = evalNomex (h (en, InputData $ RadioData $ fst $ cs!!i)) rn
execInputHandler (UCheckboxData is) (EH en rn (InputEv (Input _ _ (Checkbox cs))) h SActive) = evalNomex (h (en, InputData $ CheckboxData $ fst <$> cs `sel` is)) rn
execInputHandler _ _ = return ()

findEvent :: EventNumber -> [EventHandler] -> Maybe EventHandler
findEvent en = find ((== en) . getL eventNumber)

--Get all event numbers of type choice (radio button)
getChoiceEvents :: State Game [EventNumber]
getChoiceEvents = do
   evs <- access events
   return $ map _eventNumber $ filter choiceEvent evs
   where choiceEvent (EH _ _ (InputEv (Input _ _ (Radio _))) _ _) = True
         choiceEvent _ = False

--Get all event numbers of type text (text field)
getTextEvents :: State Game [EventNumber]
getTextEvents = do
   evs <- access events
   return $ map _eventNumber $ filter choiceEvent evs
   where choiceEvent (EH _ _ (InputEv (Input _ _ Text)) _ _) = True
         choiceEvent _ = False

evProposeRule :: RuleInfo -> Evaluate Bool
evProposeRule rule = do
   rs <- access rules
   case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
      Nothing -> do
         rules %= (rule:)
         triggerEvent_ (RuleEv Proposed) (RuleData rule)
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
         triggerEvent_ (RuleEv Activated) (RuleData r)
         return True

evRejectRule :: RuleNumber -> RuleNumber -> Evaluate Bool
evRejectRule rn by = do
   rs <- access rules
   case find (\r -> _rNumber r == rn && _rStatus r /= Reject) rs of
      Nothing -> return False
      Just r -> do
         let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Reject, _rAssessedBy = Just by} rs
         rules ~= newrules
         triggerEvent_ (RuleEv Rejected) (RuleData r)
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
         triggerEvent_ (RuleEv Added) (RuleData rule)
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
         triggerEvent_ (RuleEv Modified) (RuleData r)
         return True

addPlayer :: PlayerInfo -> Evaluate Bool
addPlayer pi = do
   pls <- access players
   let exists = any (((==) `on` _playerNumber) pi) pls
   unless exists $ do
       players %= (pi:)
       triggerEvent_ (Player Arrive) (PlayerData pi)
   return $ not exists

evDelPlayer :: PlayerNumber -> Evaluate Bool
evDelPlayer pn = do
   g <- get
   case find ((== pn) . getL playerNumber) (_players g) of
      Nothing -> do
         tracePN pn "not in game!"
         return False
      Just pi -> do
         players %= filter ((/= pn) . getL playerNumber)
         triggerEvent_ (Player Leave) (PlayerData pi)
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


evTriggerTime :: UTCTime -> Evaluate Bool
evTriggerTime t = triggerEvent (Time t) (TimeData t)


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

logPlayer :: PlayerNumber -> String -> Evaluate ()
logPlayer pn = log (Just pn)

logAll :: String -> Evaluate ()
logAll = log Nothing

log :: Maybe PlayerNumber -> String -> Evaluate ()
log mpn s = do
   time <- access currentTime
   void $ logs %= (Log mpn time s : )

--remove the ErrorT layer from the Evaluate monad stack.
runEvalError :: Maybe PlayerNumber -> Evaluate a -> State Game ()
runEvalError pn egs = do
   e <- runErrorT egs
   case e of
      Right _ -> return ()
      Left e -> do
         tracePN (fromMaybe 0 pn) $ "Error: " ++ e
         void $ runErrorT $ log pn "Error: "

-- | Show instance for Game
-- showing a game involves evaluating some parts (such as victory and outputs)
instance Show Game where
   show g@(Game { _gameName, _rules, _players, _variables, _events, _victory, _currentTime}) =
        "Game Name = "      ++ show _gameName ++
        "\n Rules = "       ++ (intercalate "\n " $ map show _rules) ++
        "\n Players = "     ++ show _players ++
        "\n Variables = "   ++ show _variables ++
        "\n Events = "      ++ show _events ++
        "\n Outputs = "     ++ show (allOutputs g) ++
        "\n Victory = "     ++ show (getVictorious g) ++
        "\n currentTime = " ++ show _currentTime ++ "\n"
