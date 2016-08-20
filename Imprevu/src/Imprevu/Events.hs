{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | All the building blocks to allow rules to build events.
module Imprevu.Events
--   onEvent, onEvent_, onEventOnce,
--   delEvent,
--   getEvents, getEvent,
--   getIntermediateResults,
--   schedule, schedule_, schedule', schedule'_,
--   getCurrentTime,
--   oneWeek, oneDay, oneHour, oneMinute,
--   timeEvent, messageEvent, victoryEvent, playerEvent, ruleEvent,
--   signalEvent, inputFormSignal,
--   liftEvent
    where

import Imprevu.Internal.Event
import Data.Typeable
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Safe

class (Typeable n, Applicative n, Monad n) => EvMgt n where
   --Events management
   onEvent         :: (Typeable a, Show a) => Event a -> ((EventNumber, a) -> n ()) -> n EventNumber
   delEvent        :: EventNumber -> n Bool
   getEvents       :: n [EventInfo n]
   sendMessage     :: (Typeable a, Show a, Eq a) => Msg a -> a -> n ()

type Msg m = Signal String m

partial :: (MonadError String n) => String -> n (Maybe a) -> n a
partial s nm = do
   m <- nm
   case m of
      Just a -> return a
      Nothing -> throwError s

-- * Events

-- | register a callback on an event, disregard the event number
onEvent_ :: (Typeable a, Show a, EvMgt n) => Event a -> (a -> n ()) -> n EventNumber
onEvent_ e h = onEvent e (\(_, d) -> h d)

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable a, Show a, EvMgt n) => Event a -> (a -> n ()) -> n EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent en >> h ed
    onEvent e handler


--getEvent :: EventNumber -> Nomex (Maybe EventInfo)
--getEvent en = find (\(EventInfo en2 _ _ evst _) -> en == en2 && evst == SActive) <$> getEvents
--
--getIntermediateResults :: EventNumber -> Nomex (Maybe [SomeData])
--getIntermediateResults en = do
--   mev <- getEvent en
--   case mev of
--      Just ev -> return $ Just $ mapMaybe getInputResult (_env ev)
--      Nothing -> return Nothing

--getInputResult :: SignalOccurence -> Maybe SomeData
--getInputResult (SignalOccurence (SignalData (Input _ _) r) _) = Just (SomeData r)
--getInputResult _ = Nothing

-- | on the provided schedule, the supplied function will be called
--schedule :: Schedule Freq -> (UTCTime -> Nomex ()) -> Nomex ()
--schedule sched f = do
--    now <- getCurrentTime
--    let next = head $ starting now sched
--    if next == now then executeAndScheduleNext f sched now
--                   else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext f sched
--
--executeAndScheduleNext :: (UTCTime -> Nomex ()) -> Schedule Freq -> UTCTime -> Nomex ()
--executeAndScheduleNext f sched now = do
--   f now
--   let rest = drop 1 $ starting now sched
--   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext f sched
--
--schedule_ :: Schedule Freq -> Nomex () -> Nomex ()
--schedule_ ts f = schedule ts (const f)
--
----at each time provided, the supplied function will be called
--schedule' :: [UTCTime] -> (UTCTime -> Nomex ()) -> Nomex ()
--schedule' sched f = do
--    let sched' = sort sched
--    now <- getCurrentTime
--    let nextMay = headMay $ filter (>=now) sched'
--    case nextMay of
--        Just next -> if next == now then executeAndScheduleNext' f sched' now
--                                    else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext' f sched'
--        Nothing -> return ()
--
--
--executeAndScheduleNext' :: (UTCTime -> Nomex ()) -> [UTCTime] -> UTCTime -> Nomex ()
--executeAndScheduleNext' f sched now = do
--   f now
--   let rest = drop 1 sched
--   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext' f sched
--
--
--schedule'_ :: [UTCTime] -> Nomex () -> Nomex ()
--schedule'_ ts f = schedule' ts (const f)

-- | get the current time as UTCTime
--getCurrentTime :: Nomex UTCTime
--getCurrentTime = CurrentTime

-- * Individual events

-- | Build an event firing at a specific time
--timeEvent :: UTCTime -> Event UTCTime
--timeEvent t = SignalEvent $ Signal t

-- | Build a event firing when the victory condition is changed
--victoryEvent :: Event VictoryInfo
--victoryEvent = SignalEvent Victory

-- | Build a event firing when a player arrives or leaves
--playerEvent :: Player -> Event PlayerInfo
--playerEvent = SignalEvent . Player

-- | Build a event firing when an action is made on a rule
--ruleEvent :: RuleEvent -> Event RuleInfo
--ruleEvent re = SignalEvent $ RuleEv re

-- | Build a message event, that can be intercepted by another rule
-- this is useful for message-passing style of communication
messageEvent :: (Typeable a, Show a) => Msg a -> Event a
messageEvent m = SignalEvent m

-- | Build a event firing immediatly, yelding the value of the Nomex
--liftEvent :: Nomex a -> Event a
--liftEvent = LiftEvent

-- | duration
oneWeek, oneDay, oneHour, oneMinute :: NominalDiffTime
oneWeek = 7 * oneDay
oneDay = 24 * oneHour
oneHour = 60 * oneMinute
oneMinute = 60

-- * internals

--inputFormSignal :: (Typeable a) => String -> (InputForm a) -> Signal a
--inputFormSignal s iform = Input s iform

signalEvent    :: (Eq s, Typeable s, Show s, Typeable e, Show e) => s -> Event e                                  -- Embed a single Signal as an Event
signalEvent = SignalEvent . Signal


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
--        --tracePN (fromMaybe 0 mpn) $ "Error: " ++ e'
         --void $ runErrorT $ log mpn "Error: "

--runSystemEval :: Evaluate n s a -> State s a
--runSystemEval eva = focus execState $ runEvalError eva
--
--runSystemEval' :: Evaluate n s a -> State s ()
--runSystemEval' eva = void $ runSystemEval eva


displayEvent :: [EventInfo n] -> EventInfo n -> String
displayEvent eis ei@(EventInfo en _ _ s envi) =
   "event num: " ++ (show en) ++
   --", remaining signals: " ++ (show $ getRemainingSignals ei eis) ++ --TODO: display also event result?
  -- ", envs: " ++ (show envi) ++
   ", status: " ++ (show s)

