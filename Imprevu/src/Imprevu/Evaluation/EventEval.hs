{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell           #-}


-- | Evaluation of the events
module Imprevu.Evaluation.EventEval where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Validation
import           Data.Typeable
import           Imprevu.Types
import           Imprevu.Evaluation.Utils
import           Prelude                     hiding (log, (.))
import           Safe
import           Debug.Trace.Helpers    (traceM)


class HasEvents n s where
   getEvents :: s -> [EventInfoN n]
   setEvents :: [EventInfoN n] -> s -> s

-- | Environment necessary for the evaluation of events
data EvalEnvN n s = EvalEnv { _evalEnv     :: s,
                             evalFunc     :: forall a. n a -> EvaluateN n s a,           -- evaluation function
                             errorHandler :: EventNumber -> String -> EvaluateN n s ()}  -- error function

-- | Environment necessary for the evaluation of Nome
type EvaluateN n s a = ExceptT String (State (EvalEnvN n s)) a

makeLenses ''EvalEnvN

events :: (HasEvents n s) => Lens' (EvalEnvN n s) [EventInfoN n]
events f (EvalEnv s g h) = fmap (\s' -> (EvalEnv (setEvents s' s) g h)) (f (getEvents s))


-- * Event triggers

-- trigger an event with an event result
triggerEvent :: (HasEvents n s, Show a, Typeable a, Show e, Typeable e, Eq a) => Signal a e -> e -> EvaluateN n s ()
triggerEvent e dat = do
   (EvalEnv s _ _) <- get
   triggerEvent' (SignalData e dat) Nothing (getEvents s)

-- trigger some specific signal
triggerEvent' :: (HasEvents n s) => SignalData -> Maybe SignalAddress -> [EventInfoN n] -> EvaluateN n s ()
triggerEvent' sd msa evs = do
   let evs' = evs -- sortBy (compare `on` _ruleNumber) evs
   eids <- mapM (getUpdatedEventInfo sd msa) evs'           -- get all the EventInfoNs updated with the field
   traceM $ "triggerEvent' eids=" ++ (show eids) ++ " sd=" ++ (show sd) ++ " msa=" ++ (show msa) ++ " evs=" ++ (show evs)
   events %= union (map fst eids)                           -- store them
   void $ mapM triggerIfComplete eids                           -- trigger the handlers for completed events

-- if the event is complete, trigger its handler
triggerIfComplete :: (EventInfoN n, Maybe SomeData) -> EvaluateN n s ()
triggerIfComplete (EventInfo en _ h SActive _, Just (SomeData val)) = case cast val of
   Just a -> do
      traceM $ "triggerIfComplete: " ++ (show a)
      eval <- gets evalFunc
      err <- gets errorHandler
      (void $ (eval $ h (en, a))) `catchError` (err en)
   Nothing -> error "Bad trigger data type"
triggerIfComplete _ = return ()


-- get update the EventInfoN updated with the signal data.
-- get the event result if all signals are completed
getUpdatedEventInfo :: SignalData -> Maybe SignalAddress -> EventInfoN n -> EvaluateN n s (EventInfoN n, Maybe SomeData)
getUpdatedEventInfo sd@(SignalData sig _) addr ei@(EventInfo _ ev _ _ envi) = do
   trs <- getEventResult ev envi
   traceM $ "getUpdatedEventInfo trs=" ++ (show trs) ++ " envi=" ++ (show envi) ++ " sig=" ++ (show sig) ++ " addr=" ++ (show addr)
   case trs of
      AccFailure rs -> case find (\(sa, (SomeSignal ss)) -> (ss === sig) && maybe True (==sa) addr) rs of -- check if our signal match one of the remaining signals
         Just (sa, _) -> do
            traceM $ "getUpdatedEventInfo sa=" ++ (show sa)
            let envi' = SignalOccurence sd sa : envi
            er <- getEventResult ev envi'                                                           -- add our event to the environment and get the result
            case er of
               AccFailure _ -> do
                 traceM $ "getUpdatedEventInfo"
                 return (env .~ envi' $ ei, Nothing)                                              -- some other signals are left to complete: add ours in the environment
               AccSuccess a -> do
                 traceM $ "getUpdatedEventInfo a=" ++ (show a)
                 return (env .~  [] $ ei, Just $ SomeData a)                                       -- event complete: return the final data result
         Nothing -> do
           traceM "getUpdatedEventInfo Nothing"
           return (ei, Nothing)                                                            -- our signal does not belong to this event.
      AccSuccess a -> return (env .~  [] $ ei, Just $ SomeData a)


-- * Evaluations

--get the signals left to be completed in an event
getRemainingSignals' :: EventInfoN n -> EvaluateN n s [(SignalAddress, SomeSignal)]
getRemainingSignals' (EventInfo _ e _ _ envi) = do
   tr <- getEventResult e envi
   return $ case tr of
      AccSuccess _ -> []
      AccFailure a -> a

getRemainingSignals :: EventInfoN n -> EvalEnvN n s -> [(SignalAddress, SomeSignal)]
getRemainingSignals ei env = join $ maybeToList $ evalState (runEvalError' (getRemainingSignals' ei)) env


-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some signals results are pending, return that list instead.
getEventResult :: EventM n a -> [SignalOccurence] -> EvaluateN n s (AccValidation [(SignalAddress, SomeSignal)] a)
getEventResult e frs = getEventResult' e frs []

-- compute the result of an event given an environment. The third argument is used to know where we are in the event tree.
getEventResult' :: EventM n a -> [SignalOccurence] -> SignalAddress -> EvaluateN n s (AccValidation [(SignalAddress, SomeSignal)] a)
getEventResult' (PureEvent a)   _   _  = return $ AccSuccess a
getEventResult'  EmptyEvent     _   _  = return $ AccFailure []
getEventResult' (SumEvent a b)  ers fa = liftM2 (<|>) (getEventResult' a ers (fa ++ [SumL])) (getEventResult' b ers (fa ++ [SumR]))
getEventResult' (AppEvent f b)  ers fa = liftM2 (<*>) (getEventResult' f ers (fa ++ [AppL])) (getEventResult' b ers (fa ++ [AppR]))
getEventResult' (LiftEvent a)   _   _  = do
   eval <- gets evalFunc
   AccSuccess <$> eval a
   --return $ AccSuccess r
getEventResult' (BindEvent a f) ers fa = do
   er <- getEventResult' a ers (fa ++ [BindL])
   case er of
      AccSuccess a' -> getEventResult' (f a') ers (fa ++ [BindR])
      AccFailure bs -> return $ AccFailure bs

getEventResult' (SignalEvent a) ers fa = return $ case lookupSignal a fa ers of
   Just r  -> AccSuccess r
   Nothing -> AccFailure [(fa, SomeSignal a)]

getEventResult' (ShortcutEvents es f) ers fa = do
  ers' <- mapM (\e -> getEventResult' e ers (fa ++ [Shortcut])) es -- get the result for each event in the list
  return $ if f (toMaybe <$> ers')                                   -- apply f to the event results that we already have
     then AccSuccess $ toMaybe <$> ers'                               -- if the result is true, we are done. Return the list of maybe results
     else AccFailure $ join $ lefts $ toEither <$> ers'                  -- otherwise, return the list of remaining fields to complete from each event


runEvalError' :: EvaluateN n s a -> State (EvalEnvN n s) (Maybe a)
runEvalError' egs = do
   e <- runExceptT egs
   case e of
      Right a -> return $ Just a
      Left e' -> error $ "error " ++ e'
         --tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
         --void $ runErrorT $ log mpn "Error: "

runEvaluate :: EvaluateN n s a -> EvalEnvN n s -> Maybe a
runEvaluate ev ee = evalState (runEvalError' ev) ee

-- find a signal occurence in an environment
lookupSignal :: (Typeable a, Typeable s, Eq s) => Signal s a -> SignalAddress -> [SignalOccurence] -> Maybe a
lookupSignal s sa envi = headMay $ mapMaybe (getSignalData s sa) envi

--get the signal data from the signal occurence
getSignalData :: (Typeable a, Typeable s, Eq s) => Signal s a -> SignalAddress -> SignalOccurence -> Maybe a
getSignalData s sa (SignalOccurence (SignalData s' res) sa') = do
  res' <- cast res
  if (sa' == sa) && (s === s') then Just res' else Nothing


execSignals :: (Show a, Show e, Typeable e, Eq e, Show d, Typeable d, Eq d, HasEvents n s) => n a -> [(Signal e d, d)] -> EvalEnvN n s -> s
execSignals r sds evalEnv = _evalEnv $ runIdentity $ flip execStateT evalEnv $ do
   res <- runExceptT $ do
      evalFunc evalEnv r
      mapM_ (\(f,d) -> triggerEvent f d) sds
   case res of
      Right a -> return a
      Left _ -> error $ show "error occured"


