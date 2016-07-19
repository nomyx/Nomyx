{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell           #-}


-- | Evaluation of the events
module Imprevu.Internal.EventEval where

import           Control.Applicative
import           Control.Category            hiding (id)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Either
import qualified Data.Foldable               as F hiding (find)
import           Data.Function               (on)
import           Data.List
import           Data.Maybe
import           Data.Todo
import           Data.Typeable
import           Imprevu.Internal.Event
import           Imprevu.Internal.EvalUtils
import           Prelude                     hiding (log, (.))
import           Safe

-- | Environment necessary for the evaluation of any nomyx expressions or events
--TODO: should the first field be a "Maybe RuleNumber"?
--Indeed an evaluation is not always performed by a rule but also by the system (in which case we currently use rule number 0)
data EvalEnv n s = EvalEnv { _events      :: [EventInfo n],
                             _execState   :: s,
                             evalFunc :: forall a. (Show a) => n a -> Evaluate n s (),       -- evaluation function
                             errorHandler :: EventNumber -> String -> Evaluate n s ()}    -- error function


-- | Environment necessary for the evaluation of Nome
type Evaluate n s a = ErrorT String (State (EvalEnv n s)) a

makeLenses ''EvalEnv

-- * Event triggers

-- trigger an event with an event result
triggerEvent :: (Signal e) => e -> (SignalDataType e) -> Evaluate n s ()
triggerEvent s dat = do
   evs <- gets _events
   triggerEvent' (SignalData s dat) Nothing evs

-- trigger some specific signal
triggerEvent' :: SignalData -> Maybe SignalAddress -> [EventInfo n] -> Evaluate n s ()
triggerEvent' sd msa evs = do
   let evs' = evs -- sortBy (compare `on` _ruleNumber) evs
   eids <- mapM (getUpdatedEventInfo sd msa) evs'           -- get all the EventInfos updated with the field
   events %= union (map fst eids)                           -- store them
   void $ mapM triggerIfComplete eids                           -- trigger the handlers for completed events

-- if the event is complete, trigger its handler
triggerIfComplete :: (EventInfo n, Maybe SomeData) -> Evaluate n s ()
triggerIfComplete (EventInfo en _ h SActive _, Just (SomeData val)) = case cast val of
   Just a -> do
      eval <- gets evalFunc
      err <- gets errorHandler
      (void $ (eval $ h (en, a))) `catchError` (err en)
   Nothing -> error "Bad trigger data type"
triggerIfComplete _ = return ()


-- get update the EventInfo updated with the signal data.
-- get the event result if all signals are completed
getUpdatedEventInfo :: SignalData -> Maybe SignalAddress -> EventInfo n -> Evaluate n s (EventInfo n, Maybe SomeData)
getUpdatedEventInfo sd@(SignalData sig _) addr ei@(EventInfo _ ev _ _ envi) = do
   trs <- getEventResult ev envi
   case trs of
      Todo rs -> case find (\(sa, ss) -> (ss == SomeSignal sig) && maybe True (==sa) addr) rs of -- check if our signal match one of the remaining signals
         Just (sa, _) -> do
            let envi' = SignalOccurence sd sa : envi
            er <- getEventResult ev envi'                                                           -- add our event to the environment and get the result
            return $ case er of
               Todo _ -> (env .~ envi' $ ei, Nothing)                                              -- some other signals are left to complete: add ours in the environment
               Done a -> (env .~  [] $ ei, Just $ SomeData a)                                       -- event complete: return the final data result
         Nothing -> return (ei, Nothing)                                                            -- our signal does not belong to this event.
      Done a -> return (env .~  [] $ ei, Just $ SomeData a)

--get the signals left to be completed in an event
getRemainingSignals' :: EventInfo n -> Evaluate n s [(SignalAddress, SomeSignal)]
getRemainingSignals' (EventInfo _ e _ _ envi) = do
   tr <- getEventResult e envi
   return $ case tr of
      Done _ -> []
      Todo a -> a


-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some signals results are pending, return that list instead.
getEventResult :: Event a -> [SignalOccurence] -> Evaluate n s (Todo (SignalAddress, SomeSignal) a)
getEventResult e frs = getEventResult' e frs []

-- compute the result of an event given an environment. The third argument is used to know where we are in the event tree.
getEventResult' :: Event e -> [SignalOccurence] -> SignalAddress -> Evaluate n s (Todo (SignalAddress, SomeSignal) e)
getEventResult' (PureEvent a)   _   _  = return $ Done a
getEventResult'  EmptyEvent     _   _  = return $ Todo []
getEventResult' (SumEvent a b)  ers fa = liftM2 (<|>) (getEventResult' a ers (fa ++ [SumL])) (getEventResult' b ers (fa ++ [SumR]))
getEventResult' (AppEvent f b)  ers fa = liftM2 (<*>) (getEventResult' f ers (fa ++ [AppL])) (getEventResult' b ers (fa ++ [AppR]))
--getEventResult' (LiftEvent a)   _   _  = do
--   evalNomexNE <- asks evalNomexNEFunc
--   r <- evalNomexNE a
--   return $ Done r
getEventResult' (BindEvent a f) ers fa = do
   er <- getEventResult' a ers (fa ++ [BindL])
   case er of
      Done a' -> getEventResult' (f a') ers (fa ++ [BindR])
      Todo bs -> return $ Todo bs

getEventResult' (SignalEvent a) ers fa = return $ case lookupSignal a fa ers of
   Just r  -> Done r
   Nothing -> Todo [(fa, SomeSignal a)]

getEventResult' (ShortcutEvents es f) ers fa = do
  (ers' :: [Todo (SignalAddress, SomeSignal) a]) <- mapM (\e -> getEventResult' e ers (fa ++ [Shortcut])) es -- get the result for each event in the list
  return $ if f (toMaybe <$> ers')                                                                     -- apply f to the event results that we already have
     then Done $ toMaybe <$> ers'                                                                        -- if the result is true, we are done. Return the list of maybe results
     else Todo $ join $ lefts $ toEither <$> ers'                                                        -- otherwise, return the list of remaining fields to complete from each event



