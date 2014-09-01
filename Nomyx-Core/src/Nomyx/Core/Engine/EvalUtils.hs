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
import Control.Applicative
import Control.Monad.Error
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Utils

-- find a field result in an environment
-- TODO simplify?
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
   show (EventInfo en rn _ _ s env) =
      "event num: " ++ (show en) ++
      ", rule num: " ++ (show rn) ++
      ", envs: " ++ (show env) ++
      ", status: " ++ (show s)
