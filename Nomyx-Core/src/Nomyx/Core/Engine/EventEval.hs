{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Evaluation of the events
module Nomyx.Core.Engine.EventEval where

import           Control.Applicative
import           Control.Category            hiding (id)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Either
import qualified Data.Foldable               as F hiding (find)
import           Data.Function               (on)
import           Data.List
import           Data.Maybe
import           Data.Todo
import           Data.Typeable
import           Language.Nomyx.Expression
import           Nomyx.Core.Engine.EvalUtils
import           Nomyx.Core.Engine.Types     hiding (_vRuleNumber)
import           Nomyx.Core.Engine.Utils
import           Prelude                     hiding (log, (.))
import           Safe


-- * Event triggers

-- trigger an event with an event result
triggerEvent :: (Typeable e, Show e) => Signal e -> e -> Evaluate ()
triggerEvent s dat = do
   evs <- use (eGame . events)
   triggerEvent' (SignalData s dat) Nothing evs

-- trigger some specific signal
triggerEvent' :: SignalData -> Maybe SignalAddress -> [EventInfo] -> Evaluate ()
triggerEvent' sd msa evs = do
   let evs' = sortBy (compare `on` _ruleNumber) evs
   eids <- mapM (liftEval . (getUpdatedEventInfo sd msa)) evs'  -- get all the EventInfos updated with the field
   (eGame . events) %= union (map fst eids)                   -- store them
   void $ mapM triggerIfComplete eids                           -- trigger the handlers for completed events

-- if the event is complete, trigger its handler
triggerIfComplete :: (EventInfo, Maybe SomeData) -> Evaluate ()
triggerIfComplete (EventInfo en rn _ h SActive _, Just (SomeData val)) = case cast val of
   Just a -> do
      evalNomex <- gets evalNomexFunc
      void $ withRN rn (evalNomex $ h (en, a)) `catchError` (errorHandler en)
   Nothing -> error "Bad trigger data type"
triggerIfComplete _ = return ()

-- get update the EventInfo updated with the signal data.
-- get the event result if all signals are completed
getUpdatedEventInfo :: SignalData -> Maybe SignalAddress -> EventInfo -> EvaluateNE (EventInfo, Maybe SomeData)
getUpdatedEventInfo sd@(SignalData sig _) addr ei@(EventInfo _ _ ev _ _ envi) = do
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
getRemainingSignals' :: EventInfo -> EvaluateNE [(SignalAddress, SomeSignal)]
getRemainingSignals' (EventInfo _ _ e _ _ envi) = do
   tr <- getEventResult e envi
   return $ case tr of
      Done _ -> []
      Todo a -> a

-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some signals results are pending, return that list instead.
getEventResult :: Event a -> [SignalOccurence] -> EvaluateNE (Todo (SignalAddress, SomeSignal) a)
getEventResult e frs = getEventResult' e frs []

-- compute the result of an event given an environment. The third argument is used to know where we are in the event tree.
getEventResult' :: Event a -> [SignalOccurence] -> SignalAddress -> EvaluateNE (Todo (SignalAddress, SomeSignal) a)
getEventResult' (PureEvent a)   _   _  = return $ Done a
getEventResult'  EmptyEvent     _   _  = return $ Todo []
getEventResult' (SumEvent a b)  ers fa = liftM2 (<|>) (getEventResult' a ers (fa ++ [SumL])) (getEventResult' b ers (fa ++ [SumR]))
getEventResult' (AppEvent f b)  ers fa = liftM2 (<*>) (getEventResult' f ers (fa ++ [AppL])) (getEventResult' b ers (fa ++ [AppR]))
getEventResult' (LiftEvent a)   _   _  = do
   evalNomexNE <- asks evalNomexNEFunc
   r <- evalNomexNE a
   return $ Done r
getEventResult' (BindEvent a f) ers fa = do
   er <- getEventResult' a ers (fa ++ [BindL])
   case er of
      Done a' -> getEventResult' (f a') ers (fa ++ [BindR])
      Todo bs -> return $ Todo bs

getEventResult' (SignalEvent a)  ers fa = return $ case lookupSignal a fa ers of
   Just r  -> Done r
   Nothing -> Todo [(fa, SomeSignal a)]

getEventResult' (ShortcutEvents es f) ers fa = do
  (ers' :: [Todo (SignalAddress, SomeSignal) a]) <- mapM (\e -> getEventResult' e ers (fa ++ [Shortcut])) es -- get the result for each event in the list
  return $ if f (toMaybe <$> ers')                                                                     -- apply f to the event results that we already have
     then Done $ toMaybe <$> ers'                                                                        -- if the result is true, we are done. Return the list of maybe results
     else Todo $ join $ lefts $ toEither <$> ers'                                                        -- otherwise, return the list of remaining fields to complete from each event


-- * Input triggers

-- trigger the input form with the input data
triggerInput :: FormField -> InputData -> SignalAddress -> EventNumber -> Evaluate ()
triggerInput ff i sa en = do
   evs <- use (eGame . events)
   let mei = find (\a -> a ^. eventNumber == en) evs
   F.forM_ mei (triggerInputSignal i sa ff)

-- trigger the input signal with the input data
triggerInputSignal :: InputData -> SignalAddress -> FormField -> EventInfo -> Evaluate ()
triggerInputSignal ide sa ff ei@(EventInfo _ _ _ _ SActive _) = do
   i <- liftEval $ findField ff sa ei
   case i of
      Just sf -> triggerInputSignal' ide sf sa ei
      Nothing -> logAll $ "Input not found, InputData=" ++ (show ide) ++ " SignalAddress=" ++ (show sa) ++ " FormField=" ++ (show ff)
triggerInputSignal _ _ _ _ = return ()

-- trigger the input signal with the input data
triggerInputSignal' :: InputData -> SomeSignal -> SignalAddress -> EventInfo -> Evaluate ()
triggerInputSignal' (TextData s)      (SomeSignal e@(Input _ _  Text))         sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
triggerInputSignal' (TextAreaData s)  (SomeSignal e@(Input _ _  TextArea))     sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
triggerInputSignal' (ButtonData)      (SomeSignal e@(Input _ _  Button))       sa ei = triggerEvent' (SignalData e ())                    (Just sa) [ei]
triggerInputSignal' (RadioData i)     (SomeSignal e@(Input _ _ (Radio cs)))    sa ei = triggerEvent' (SignalData e (fst $ cs!!i))         (Just sa) [ei]
triggerInputSignal' (CheckboxData is) (SomeSignal e@(Input _ _ (Checkbox cs))) sa ei = triggerEvent' (SignalData e (fst <$> cs `sel` is)) (Just sa) [ei]
triggerInputSignal' _ _ _ _ = return ()


-- | Get the form field at a certain address
findField :: FormField -> SignalAddress -> EventInfo -> EvaluateNE (Maybe SomeSignal)
findField ff addr (EventInfo _ _ e _ _ envi) = findField' addr e envi ff

findField' :: SignalAddress -> Event e -> [SignalOccurence] -> FormField -> EvaluateNE (Maybe SomeSignal)
findField' []         (SignalEvent f)    _   ff = return $ do
   ff' <- getFormField (SomeSignal f)
   guard (ff' == ff)
   return $ SomeSignal f
findField' (SumL:as)  (SumEvent e1 _)  envi ff = findField' as e1 (filterPath SumL envi) ff
findField' (SumR:as)  (SumEvent _ e2)  envi ff = findField' as e2 (filterPath SumR envi) ff
findField' (AppL:as)  (AppEvent e1 _)  envi ff = findField' as e1 (filterPath AppL envi) ff
findField' (AppR:as)  (AppEvent _ e2)  envi ff = findField' as e2 (filterPath AppR envi) ff
findField' (BindL:as) (BindEvent e1 _) envi ff = findField' as e1 (filterPath BindL envi) ff
findField' (BindR:as) (BindEvent e1 f) envi ff = do
   ter <- getEventResult e1 (filterPath BindL envi)
   case ter of
      Done e2 -> findField' as (f e2) (filterPath BindR envi) ff
      Todo _  -> return Nothing
findField' (Shortcut:as) (ShortcutEvents es _) envi ff = do
   msfs <- mapM (\e-> findField' as e envi ff) es
   return $ headMay $ catMaybes msfs  -- returning the first field that matches

findField' fa _ _ _ = error $ "findField: wrong field address: " ++ (show fa)

-- | removes one element of signal address for all signal occurences
filterPath :: SignalAddressElem -> [SignalOccurence] -> [SignalOccurence]
filterPath fa envi = mapMaybe f envi where
   f (SignalOccurence sd (fa':fas)) | fa == fa' = Just $ SignalOccurence sd fas
   f fr = Just fr

getFormField :: SomeSignal -> Maybe FormField
getFormField (SomeSignal (Input pn s (Radio choices)))    = Just $ RadioField    pn s (zip [0..] (snd <$> choices))
getFormField (SomeSignal (Input pn s Text))               = Just $ TextField     pn s
getFormField (SomeSignal (Input pn s TextArea))           = Just $ TextAreaField pn s
getFormField (SomeSignal (Input pn s Button))             = Just $ ButtonField   pn s
getFormField (SomeSignal (Input pn s (Checkbox choices))) = Just $ CheckboxField pn s (zip [0..] (snd <$> choices))
getFormField _ = Nothing
