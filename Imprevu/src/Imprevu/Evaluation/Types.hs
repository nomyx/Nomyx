{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell           #-}

module Imprevu.Evaluation.Types where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Imprevu.Types

data EvalEnvN n s = EvalEnv { _evalEnv     :: s,
                              _evalConf    :: EvalConfN n s}

data EvalConfN n s = EvalConf { getEvents     :: s -> [EventInfoN n],
                                setEvents     :: [EventInfoN n] -> s -> s,
                                _evalFunc     :: forall a. n a -> EvaluateN n s a,           -- evaluation function
                                _errorHandler :: EventNumber -> String -> EvaluateN n s (),  -- error function
                                _withEvent    :: EventInfoN n -> EvaluateN n s () -> EvaluateN n s ()}  -- error function

-- | Environment necessary for the evaluation of Nome
type EvaluateN n s a = ExceptT String (State (EvalEnvN n s)) a

makeLenses ''EvalEnvN
makeLenses ''EvalConfN

events :: Lens' (EvalEnvN n s) [EventInfoN n]
events f ee@(EvalEnv s (EvalConf ge se _ _ _)) = fmap (\s' -> ee{_evalEnv = se s' s}) (f $ ge s)


