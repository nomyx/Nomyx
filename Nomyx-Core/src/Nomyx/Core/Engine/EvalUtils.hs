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
import Control.Category
import Data.Typeable
import Data.Lens
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Monad.Error
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Utils
import Data.Todo

-- compute the result of an event given an environment.
-- in the case the event cannot be computed because some fields results are pending, return that list instead.
getEventResult :: Event a -> [FieldResult] -> Todo (FieldAddress, SomeField) a
getEventResult e frs = getEventResult' e frs []

getEventResult' :: Event a -> [FieldResult] -> FieldAddress -> Todo (FieldAddress, SomeField) a
getEventResult' (PureEvent a)  _   _  = Done a
getEventResult' EmptyEvent     _   _  = Todo []
getEventResult' (SumEvent a b) ers fa = getEventResult' a ers (fa ++ [SumL]) <|> getEventResult' b ers (fa ++ [SumR])
getEventResult' (AppEvent f b) ers fa = getEventResult' f ers (fa ++ [AppL]) <*> getEventResult' b ers (fa ++ [AppR])
getEventResult' (BindEvent a f) ers fa = case (getEventResult' a ers (fa ++ [BindL])) of
   Done a' -> getEventResult' (f a') ers (fa ++ [BindR])
   Todo bs -> Todo bs
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
lookupField fi fa ((FieldResult a r fa1) : ers) = case (cast (a,r)) of
   Just (a',r') -> if (a' == fi && maybe True (== fa) fa1)
                      then Just r'
                      else lookupField fi fa ers
   Nothing      -> lookupField fi fa ers


errorHandler :: EventNumber -> String -> Evaluate ()
errorHandler en s = do
   rn <- access eRuleNumber
   logAll $ "Error in rule " ++ show rn ++ " (triggered by event " ++ show en ++ "): " ++ s



logPlayer :: PlayerNumber -> String -> Evaluate ()
logPlayer pn = log (Just pn)

logAll :: String -> Evaluate ()
logAll = log Nothing

log :: Maybe PlayerNumber -> String -> Evaluate ()
log mpn s = focusGame $ do
   time <- access currentTime
   void $ logs %= (Log mpn time s : )

liftEval :: EvaluateNE a -> Evaluate a
liftEval r = runReader r <$> get

--extract the game state from an Evaluate
--knowing the rule number performing the evaluation (0 if by the system)
--and the player number to whom display errors (set to Nothing for all players)
--TODO: clean
runEvalError :: RuleNumber -> (Maybe PlayerNumber) -> Evaluate a -> State Game ()
runEvalError rn mpn egs = modify (\g -> _eGame $ execState (runEvalError' mpn egs) (EvalEnv rn g))

runEvalError' :: (Maybe PlayerNumber) -> Evaluate a -> State EvalEnv ()
runEvalError' mpn egs = do
   e <- runErrorT egs
   case e of
      Right _ -> return ()
      Left e' -> do
         tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
         void $ runErrorT $ log mpn "Error: "

runSystemEval :: PlayerNumber -> Evaluate a -> State Game ()
runSystemEval pn e = runEvalError 0 (Just pn) e

runSystemEval' :: Evaluate a -> State Game ()
runSystemEval' e = runEvalError 0 Nothing e

focusGame :: State Game a -> Evaluate a
focusGame = lift . (focus eGame)

accessGame :: Lens Game a -> Evaluate (a, PlayerNumber)
accessGame l = do
   a <- access (eGame >>> l)
   pn <- access eRuleNumber
   return (a, pn)

--replace temporarily the rule number used for evaluation
withRN :: RuleNumber -> Evaluate a -> Evaluate a
withRN rn eval = do
   oldRn <- gets _eRuleNumber
   eRuleNumber ~= rn
   a <- eval
   eRuleNumber ~= oldRn
   return a

instance Eq SomeField where
  (SomeField e1) == (SomeField e2) = e1 === e2


instance Show EventInfo where
   show (EventInfo en rn e _ s env) =
      "event num: " ++ (show en) ++
      ", rule num: " ++ (show rn) ++
      -- ", event fields: " ++ (show $ getEventFields e env) ++
      ", envs: " ++ (show env) ++
      ", status: " ++ (show s)
