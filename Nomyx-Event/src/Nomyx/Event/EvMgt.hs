{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE RankNTypes             #-}

-- | This module contains the type definitions necessary to build a Nomic rule.
module Nomyx.Event.EvMgt where

import           Control.Applicative hiding (Const)
import           Control.Lens
import           Control.Monad.Error
import           Control.Shortcut
import           Data.Data           (Data)
import           Data.Time
import           Data.List
import           Data.Typeable
import           GHC.Generics
import           System.Random
import           Nomyx.Event.Events
import           Nomyx.Event.Internal.EventEval
import           Nomyx.Event.Internal.Utils
import           Control.Monad.State hiding (execState)
import           Control.Lens
import           Data.Todo


-- * Nomyx Expression

class (Typeable n, Monad n, Applicative n) => EvMgt n where
   --Events management
   onEvent         :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> n ()) -> n EventNumber
   delEvent        :: EventNumber -> n Bool
   getEvents       :: n [EventInfo n]
   sendMessage     :: (Typeable a, Show a) => Msg a -> a -> n ()
   currentTime     :: n UTCTime

data Msg m     = Msg String deriving (Typeable, Show, Eq)

-- * Evaluation

-- | evaluate an effecful expression.

--evOnEvent :: (Typeable e, Show e, EvMgt n) => Event e -> ((EventNumber, e) -> n ()) -> Evaluate n s EventNumber
--evOnEvent ev h = do
--   evs <- use events
--   let en = getFreeNumber (map _eventNumber evs)
--   events %= (EventInfo en ev h SActive [] : )
--   return en

--evSendMessage :: (Typeable a, Show a) => Msg a -> a -> Evaluate ()
--evSendMessage m = triggerEvent (Signal m)
--
--evDelEvent :: EventNumber -> Evaluate Bool
--evDelEvent en = do
--   evs <- use events
--   case find ((== en) . getL eventNumber) evs of
--      Nothing -> return False
--      Just eh -> case _evStatus eh of
--         SActive -> do
--            events .= replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs
--            return True
--         SDeleted -> return False
--
--evTriggerTime :: UTCTime -> Evaluate ()
--evTriggerTime t = triggerEvent (Time t) t

getRemainingSignals :: EventInfo n -> [EventInfo n] -> [(SignalAddress, SomeSignal)]
getRemainingSignals (EventInfo _ e _ _ en) eis = undefined  --case runEvaluate eis $ runEvalError (getEventResult e en) of
--   Done _ -> []
--   Todo a -> a


--extract the game state from an Evaluate
--knowing the rule number performing the evaluation (0 if by the system)
--and the player number to whom display errors (set to Nothing for all players)
--TODO: clean
--runEvalError :: Evaluate n s a -> State (EvalEnv n s) a
--runEvalError eva = undefined --modify (\g -> _eGame $ execState (runEvalError' mpn egs) (EvalEnv rn g evalNomex evalNomexNE))
--
--runEvalError' :: Evaluate a -> State (EvalEnv n s) ()
--runEvalError' eva = do
--   e <- runErrorT eva
--   case e of
--      Right _ -> return ()
--      Left e' -> undefined
--         --tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
--         --void $ runErrorT $ log mpn "Error: "
--
--runSystemEval :: Evaluate n s a -> State s a
--runSystemEval eva = focus execState $ runEvalError eva
--
--runSystemEval' :: Evaluate n s a -> State s ()
--runSystemEval' eva = void $ runSystemEval eva


displayEvent :: [EventInfo n] -> EventInfo n -> String
displayEvent eis ei@(EventInfo en _ _ s envi) =
   "event num: " ++ (show en) ++
   ", remaining signals: " ++ (show $ getRemainingSignals ei eis) ++ --TODO: display also event result?
   ", envs: " ++ (show envi) ++
   ", status: " ++ (show s)
