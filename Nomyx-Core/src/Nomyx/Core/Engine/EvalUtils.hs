{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Nomyx.Core.Engine.EvalUtils where

import Prelude hiding ((.), log)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Typeable
import Data.Lens
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Monad.Error (ErrorT(..))
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Utils
import Data.Todo

-- update the EventInfo with the field data
updateEventInfo :: (Typeable a, Show a) => Field a -> a -> EventInfo -> (EventInfo, Maybe SomeData)
updateEventInfo field dat ei@(EventInfo _ _ ev _ _ envi) =
   if (SomeField field) `elem` (getEventFields ev envi)                -- if the field if found among the remaining fields of the event
      then case getEventResult ev (eventRes : envi) of                 -- then check the event with that field result included
         Todo _ -> (env ^=  (eventRes : envi) $ ei, Nothing)           -- some fields are left to complete: add ours in the environment
         Done a -> (env ^=  []                $ ei, Just $ SomeData a) -- the event is now complete: empty the environment and set the handler to be triggered
      else         (ei,                             Nothing)           -- field not found: do nothing
   where eventRes = EventEnv field dat


-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some fields results are pending, return that list instead.
getEventResult :: Event a -> [EventEnv] -> Todo SomeField a
getEventResult (PureEvent a)  _   = Done a
getEventResult EmptyEvent     _   = Todo []
getEventResult (SumEvent a b) ers = (getEventResult a ers) <|> (getEventResult b ers)
getEventResult (AppEvent f b) ers = (getEventResult f ers) <*> (getEventResult b ers)
getEventResult (BaseEvent a)  ers = case lookupField a ers of
   Just r  -> Done r
   Nothing -> Todo [SomeField a]
getEventResult (ShortcutEvents es f) ers =
  let res = partitionEithers $ toEither <$> map (flip getEventResult ers) es
  in case f (snd res) of
       Just a  -> Done a
       Nothing -> Todo $ join $ fst res

-- find a field result in an environment
lookupField :: Typeable a => Field a -> [EventEnv] -> Maybe a
lookupField _ [] = Nothing
lookupField be (EventEnv a r : ers) = case (cast (a,r)) of
   Just (a',r') -> if (a' == be) then Just r' else lookupField be ers
   Nothing      -> lookupField be ers

--get the fields lefft to be completed in an event
getEventFields :: Event a -> [EventEnv] -> [SomeField]
getEventFields e er = case (getEventResult e er) of
   Done _ -> []
   Todo a -> a

errorHandler :: RuleNumber -> EventNumber -> String -> Evaluate ()
errorHandler rn en s = logAll $ "Error in rule " ++ show rn ++ " (triggered by event " ++ show en ++ "): " ++ s

getInput :: EventInfo -> InputNumber -> Maybe SomeField
getInput (EventInfo _ _ ev _ _ env) inn = find isInput (getEventFields ev env) where
      isInput (SomeField (Input (Just n) _ _ _)) | n == inn = True
      isInput _ = False


logPlayer :: PlayerNumber -> String -> Evaluate ()
logPlayer pn = log (Just pn)

logAll :: String -> Evaluate ()
logAll = log Nothing

log :: Maybe PlayerNumber -> String -> Evaluate ()
log mpn s = do
   time <- access currentTime
   void $ logs %= (Log mpn time s : )

liftEval :: Reader Game a -> Evaluate a
liftEval r = runReader r <$> get

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
indexInputs e = snd $ indexInputs' 0 e

indexInputs' :: Int -> Event a -> (Int, Event a)
indexInputs' n (BaseEvent (Input _ pn s ifo)) = (n+1, BaseEvent (Input (Just n) pn s ifo))
indexInputs' n (BaseEvent a)  = (n, BaseEvent a)
indexInputs' n (PureEvent a)  = (n, PureEvent a)
indexInputs' n EmptyEvent     = (n, EmptyEvent)
indexInputs' n (SumEvent a b) = (n2, SumEvent e1 e2) where
   (n1, e1) = indexInputs' n a
   (n2, e2) = indexInputs' n1 b
indexInputs' n (AppEvent a b) = (n2, AppEvent e1 e2) where
   (n1, e1) = indexInputs' n a
   (n2, e2) = indexInputs' n1 b
indexInputs' n (ShortcutEvents as f) = (n1, ShortcutEvents bs f) where
   (n1, bs) = mapAccumL indexInputs' n as


instance Eq SomeField where
  (SomeField e1) == (SomeField e2) = e1 === e2


instance Show EventInfo where
   show (EventInfo en rn e _ s env) =
      "event num: " ++ (show en) ++
      ", rule num: " ++ (show rn) ++
      ", event fields: " ++ (show $ getEventFields e env) ++
      ", envs: " ++ (show env) ++
      ", status: " ++ (show s)
