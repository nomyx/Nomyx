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

import Imprevu.Event
import Imprevu.SysMgt
import Data.Typeable
import Control.Monad.Error
import Data.Time hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Data.List
import Safe

class (Typeable n, Applicative n, Monad n) => EvMgt n where
   --Events management
   onEvent         :: (Typeable a, Show a) => Event n a -> ((EventNumber, a) -> n ()) -> n EventNumber
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
onEvent_ :: (Typeable a, Show a, EvMgt n) => Event n a -> (a -> n ()) -> n EventNumber
onEvent_ e h = onEvent e (\(_, d) -> h d)

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable a, Show a, EvMgt n) => Event n a -> (a -> n ()) -> n EventNumber
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
schedule :: (EvMgt n, SysMgt n) => Schedule Freq -> (UTCTime -> n ()) -> n ()
schedule sched f = do
    now <- getCurrentTime
    let next = head $ starting now sched
    if next == now then executeAndScheduleNext f sched now
                   else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext f sched

executeAndScheduleNext :: (EvMgt n) => (UTCTime -> n ()) -> Schedule Freq -> UTCTime -> n ()
executeAndScheduleNext f sched now = do
   f now
   let rest = drop 1 $ starting now sched
   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext f sched

schedule_ :: (EvMgt n, SysMgt n) => Schedule Freq -> n () -> n ()
schedule_ ts f = schedule ts (const f)

--at each time provided, the supplied function will be called
schedule' :: (EvMgt n, SysMgt n) => [UTCTime] -> (UTCTime -> n ()) -> n ()
schedule' sched f = do
   let sched' = sort sched
   now <- getCurrentTime
   let nextMay = headMay $ filter (>=now) sched'
   case nextMay of
       Just next -> if next == now then executeAndScheduleNext' f sched' now
                                  else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext' f sched'
       Nothing -> return ()

executeAndScheduleNext' :: (EvMgt n) => (UTCTime -> n ()) -> [UTCTime] -> UTCTime -> n ()
executeAndScheduleNext' f sched now = do
   f now
   let rest = drop 1 sched
   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext' f sched


--schedule'_ :: [UTCTime] -> Nomex () -> Nomex ()
--schedule'_ ts f = schedule' ts (const f)

-- * Individual events

-- | Build an event firing at a specific time
timeEvent :: UTCTime -> Event n UTCTime
timeEvent t = SignalEvent $ Signal t

-- | Build a message event, that can be intercepted by another rule
-- this is useful for message-passing style of communication
messageEvent :: (Typeable a, Show a, Eq a) => Msg a -> Event n a
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

 -- Embed a single Signal as an Event
signalEvent    :: (Eq s, Typeable s, Show s, Typeable e, Show e, Eq e) => s -> Event n e
signalEvent = SignalEvent . Signal

-- Embed a single Signal as an Event
inputEvent    :: (Typeable e, Show e, Eq e) => Input e -> Event n e
inputEvent = SignalEvent . InputS

displayEvent :: [EventInfo n] -> EventInfo n -> String
displayEvent eis ei@(EventInfo en _ _ s envi) =
   "event num: " ++ (show en) ++
   --", remaining signals: " ++ (show $ getRemainingSignals ei eis) ++ --TODO: display also event result?
  -- ", envs: " ++ (show envi) ++
   ", status: " ++ (show s)

