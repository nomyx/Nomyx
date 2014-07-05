{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | additional tools for evaluation
module Nomyx.Core.Engine.EvalUtils where

import Prelude hiding ((.), log)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
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

-- update the EventInfo with the field result
updateEventInfo :: FieldResult -> EventInfo -> (EventInfo, Maybe SomeData)
updateEventInfo (FieldResult field dat addr) ei@(EventInfo _ _ ev _ _ envi) =
   if (SomeField field) `elem` (map snd $ getEventFields ev envi)      -- if the field if found among the remaining fields of the event
      then case getEventResult ev (eventRes : envi) of                 -- then check the event with that field result included
         Todo _ -> (env ^=  (eventRes : envi) $ ei, Nothing)           -- some fields are left to complete: add ours in the environment
         Done a -> (env ^=  []                $ ei, Just $ SomeData a) -- the event is now complete: empty the environment and output the result
      else         (ei,                             Nothing)           -- field not found: do nothing
   where eventRes = FieldResult field dat addr

-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some fields results are pending, return that list instead.
getEventResult :: Event a -> [FieldResult] -> Todo (FieldAddress, SomeField) a
getEventResult e frs = getEventResult' e frs []

getEventResult' :: Event a -> [FieldResult] -> FieldAddress -> Todo (FieldAddress, SomeField) a
getEventResult' (PureEvent a)  _   _  = Done a
getEventResult' EmptyEvent     _   _  = Todo []
getEventResult' (SumEvent a b) ers fa = getEventResult' a ers (fa ++ [L]) <|> getEventResult' b ers (fa ++ [R])
getEventResult' (AppEvent f b) ers fa = getEventResult' f ers (fa ++ [L]) <*> getEventResult' b ers (fa ++ [R])
getEventResult' (BaseEvent a)  ers fa = case lookupField a fa ers of
   Just r  -> Done r
   Nothing -> Todo [(fa, SomeField a)]
getEventResult' (ShortcutEvents es f) ers fa =
  let res = partitionEithers $ toEither <$> map (\i -> getEventResult' (es!!i) ers (fa ++ [Index i])) [0.. (length es -1)]
  in case f (snd res) of
       Just a  -> Done a
       Nothing -> Todo $ join $ fst res

-- find a field result in an environment
lookupField :: Typeable a => Field a -> FieldAddress -> [FieldResult] -> Maybe a
lookupField _ _ [] = Nothing
lookupField be fa ((FieldResult a r fa1) : ers) = case (cast (a,r)) of
   Just (a',r') -> if (a' == be && maybe True (== fa) fa1) then Just r' else lookupField be fa ers
   Nothing      -> lookupField be fa ers

--get the fields left to be completed in an event
getEventFields :: Event a -> [FieldResult] -> [(FieldAddress, SomeField)]
getEventFields e er = case (getEventResult e er) of
   Done _ -> []
   Todo a -> a

errorHandler :: RuleNumber -> EventNumber -> String -> Evaluate ()
errorHandler rn en s = logAll $ "Error in rule " ++ show rn ++ " (triggered by event " ++ show en ++ "): " ++ s

--TODO: should we check that the field is not already completed?
getInput :: EventInfo -> FieldAddress -> Maybe SomeField
getInput (EventInfo _ _ ev _ _ _) addr = findField addr ev

findField :: FieldAddress -> Event a -> Maybe SomeField
findField [] (BaseEvent field) = Just $ SomeField field
findField (L:as) (SumEvent e1 _) = findField as e1
findField (R:as) (SumEvent _ e2) = findField as e2
findField (L:as) (AppEvent e1 _) = findField as e1
findField (R:as) (AppEvent _ e2) = findField as e2
findField ((Index i):as) (ShortcutEvents es _) = findField as (es!!i)
findField _ _ = error "findField: wrong field address"

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

instance Eq SomeField where
  (SomeField e1) == (SomeField e2) = e1 === e2


instance Show EventInfo where
   show (EventInfo en rn e _ s env) =
      "event num: " ++ (show en) ++
      ", rule num: " ++ (show rn) ++
      ", event fields: " ++ (show $ getEventFields e env) ++
      ", envs: " ++ (show env) ++
      ", status: " ++ (show s)
