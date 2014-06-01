{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
import Control.Category hiding (id)
import Control.Applicative
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Error.Class (MonadError(..))
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Game
import Nomyx.Core.Engine.Utils
import Safe

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
   events %= (EventInfo en rn (indexInputs event) handler SActive [] : )
   return en

evalNomex (DelEvent en) _ = evDelEvent en

evalNomex (SendMessage (Msg id) myData) _ = triggerEvent (Message (Msg id)) myData

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
   triggerEvent Victory (VictoryCond rn ps)

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
triggerEvent :: (Typeable e, Show e) => Field e -> e -> Evaluate ()
triggerEvent e dat = do
   evs <- access events
   let evs' = map (updateEH e dat) evs
   events ~= map fst evs'
   mapM triggerIfComplete evs'
   return ()

-- update the Event handlers
updateEH :: (Typeable a, Show a) => Field a -> a -> EventInfo -> (EventInfo, Maybe SomeRes)
updateEH f dat eh@(EventInfo en rn ev h st env) = case getEventEither ev (EventRes f dat : env)  of
   BE (Left fs) -> if isJust $ find (eqSomeField (SomeField f)) fs
      then (EventInfo en rn ev h st (EventRes f dat : env), Nothing)
      else (eh, Nothing)
   BE (Right a) -> (EventInfo en rn ev h st [], Just $ SomeRes a)

data SomeRes = forall e. (Typeable e, Show e) => SomeRes e

triggerIfComplete :: (EventInfo, Maybe SomeRes) -> Evaluate ()
triggerIfComplete (EventInfo en rn _ h SActive _, Just (SomeRes val)) = do
   case (cast val) of
      Just a -> do
         let exp = h (en, a)
         (evalNomex exp rn) `catchError` (errorHandler rn en)
      Nothing -> error "Bad trigger value type"
triggerIfComplete _ = return ()

eqSomeField :: SomeField -> SomeField -> Bool
eqSomeField (SomeField e1) (SomeField e2) = e1 === e2

getEventEither :: Event a -> [EventRes] -> BEither [SomeField] a
getEventEither (PureEvent a)      _   = BE (Right a)
getEventEither EmptyEvent         _   = BE (Left [])
getEventEither (SumEvent a b)     ers = (getEventEither a ers) <|> (getEventEither b ers)
getEventEither (ProductEvent f b) ers = (getEventEither f ers) <*> (getEventEither b ers)
getEventEither (BaseEvent a)      ers = case lookupField a ers of
   Just r  -> BE (Right r)
   Nothing -> BE (Left [SomeField a])

lookupField :: Typeable a => Field a -> [EventRes] -> Maybe a
lookupField _ [] = Nothing
lookupField be ((EventRes a r):ers) = case (cast (a,r)) of
   Just (a',r') -> if (a' == be) then Just r' else lookupField be ers
   Nothing      -> lookupField be ers

getEventFields :: Event a -> [EventRes] -> [SomeField]
getEventFields e er = case (getEventEither e er) of
   BE (Right _) -> []
   BE (Left a) -> a

getEventValue :: Event a -> [EventRes] -> Maybe a
getEventValue e er = case (getEventEither e er) of
   BE (Right a) -> Just a
   BE (Left _) -> Nothing

errorHandler :: RuleNumber -> EventNumber -> String -> Evaluate ()
errorHandler rn en s = logAll $ "Error in rule " ++ show rn ++ " (triggered by event " ++ show en ++ "): " ++ s

-- trigger the input event with the input data
triggerInput :: EventNumber -> InputNumber -> UInputData -> Evaluate ()
triggerInput en inn ir = do
   evs <- access events
   let filtered = filter ((== en) . getL eventNumber) evs
   mapM_ (execInputHandler' ir inn) filtered

execInputHandler' :: UInputData -> InputNumber -> EventInfo -> Evaluate ()
execInputHandler' ir inn eh = do
    case (getInput eh inn) of
       Just sf -> execInputHandler ir sf
       Nothing -> error "Input not found"

getInput :: EventInfo -> InputNumber -> Maybe SomeField
getInput (EventInfo _ _ ev _ _ env) inn = headMay $ filter isInput (getEventFields ev env) where
      isInput (SomeField (InputEv (Just n) _ _ _)) | n == inn = True
      isInput _ = False

-- execute the event handler using the data received from user
execInputHandler :: UInputData -> SomeField -> Evaluate ()
execInputHandler (UTextData s)      (SomeField e@(InputEv _ _ _ (Text)))        = triggerEvent e s
execInputHandler (UTextAreaData s)  (SomeField e@(InputEv _ _ _ (TextArea)))    = triggerEvent e s
execInputHandler (UButtonData)      (SomeField e@(InputEv _ _ _ (Button)))      = triggerEvent e ()
execInputHandler (URadioData i)     (SomeField e@(InputEv _ _ _ (Radio cs)))    = triggerEvent e (fst $ cs!!i)
execInputHandler (UCheckboxData is) (SomeField e@(InputEv _ _ _ (Checkbox cs))) = triggerEvent e (fst <$> cs `sel` is)
execInputHandler _ _ = return ()

findEvent :: EventNumber -> [EventInfo] -> Maybe EventInfo
findEvent en = find ((== en) . getL eventNumber)

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

addPlayer :: PlayerInfo -> Evaluate Bool
addPlayer pi = do
   pls <- access players
   let exists = any (((==) `on` _playerNumber) pi) pls
   unless exists $ do
       players %= (pi:)
       triggerEvent (Player Arrive) pi
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

-- Put an index on every input fields
indexInputs :: Event a -> Event a
indexInputs e = fst $ indexInputs' 0 e

indexInputs' :: Int -> Event a -> (Event a, Int)
indexInputs' n (BaseEvent (InputEv _ pn s ifo)) = (BaseEvent (InputEv (Just n) pn s ifo), n+1)
indexInputs' n (BaseEvent a) = (BaseEvent a, n)
indexInputs' n (PureEvent a) = (PureEvent a, n)
indexInputs' n EmptyEvent = (EmptyEvent, n)
indexInputs' n (SumEvent a b) = (SumEvent e1 e2, n2) where
   (e1, n1) = indexInputs' n a
   (e2, n2) = indexInputs' n1 b
indexInputs' n (ProductEvent a b) = (ProductEvent e1 e2, n2) where
   (e1, n1) = indexInputs' n a
   (e2, n2) = indexInputs' n1 b

newtype BEither a b = BE (Either a b) deriving (Show, Eq, Typeable)
bLeft  = BE . Left
bRight = BE . Right

instance Alternative (BEither [a]) where
   empty        = bLeft []
   BE (Left a) <|> BE (Left b) = bLeft $ a ++ b
   BE (Left _) <|> n = n
   m      <|> _ = m

instance Applicative (BEither [a]) where
   pure = BE . Right
   BE (Left  a)  <*>  BE (Left b)  =  BE (Left (a ++ b))
   BE (Left  a)  <*>  _  =  BE (Left a)
   BE (Right f)  <*>  r  =  fmap f r

instance Functor (BEither a) where
   fmap _ (BE (Left x))  = BE (Left x)
   fmap f (BE (Right y)) = BE (Right (f y))

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


instance Show EventInfo where
   show (EventInfo en rn e _ s env) =
      "event num: " ++ (show en) ++
      ", rule num: " ++ (show rn) ++
      ", event fields: " ++ (show $ getEventFields e env) ++
      ", envs: " ++ (show env) ++
      ", status: " ++ (show s)
