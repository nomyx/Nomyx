{-# LANGUAGE ScopedTypeVariables #-}

-- | All the building blocks to allow rules to build events.
module Event.Nomyx.Events (
   onEvent, onEvent_, onEventOnce,
   delEvent,
   getEvents, getEvent,
   getIntermediateResults,
   schedule, schedule_, schedule', schedule'_,
   getCurrentTime,
   oneWeek, oneDay, oneHour, oneMinute,
   timeEvent, messageEvent, victoryEvent, playerEvent, ruleEvent,
   signalEvent, inputFormSignal,
   liftEvent
   ) where

import Data.Typeable
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Safe
import Event.Nomyx.Expression

-- * Events

-- | register a callback on an event
onEvent :: (Typeable e, Show e, Monad m) => Event e -> ((EventNumber, e) -> m ()) -> m EventNumber
onEvent = OnEvent

-- | register a callback on an event, disregard the event number
onEvent_ :: (Typeable e, Show e, Monad m) => Event e -> (e -> m ()) -> m EventNumber
onEvent_ e h = onEvent e (\(_, d) -> h d)

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable e, Show e, Monad m) => Event e -> (e -> m ()) -> m EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent en >> h ed
    OnEvent e handler

delEvent :: (Monad m) => EventNumber -> m Bool
delEvent = DelEvent

getEvents :: (Monad n) => n [EventInfo]
getEvents = GetEvents

getEvent :: (Monad n) => EventNumber -> n (Maybe EventInfo)
getEvent en = find (\(EventInfo en2 _ _ _ evst _) -> en == en2 && evst == SActive) <$> getEvents

getIntermediateResults :: (Monad n) => EventNumber -> n (Maybe [(PlayerNumber, SomeData)])
getIntermediateResults en = do
   mev <- getEvent en
   case mev of
      Just ev -> return $ Just $ mapMaybe getInputResult (_env ev)
      Nothing -> return Nothing

getInputResult :: SignalOccurence -> Maybe SomeData
getInputResult (SignalOccurence (SignalData (Input _ _) r) _) = Just (SomeData r)
getInputResult _ = Nothing

-- | on the provided schedule, the supplied function will be called
--schedule :: (Monad m) => Schedule Freq -> (UTCTime -> m ()) -> m ()
--schedule sched f = do
--    now <- liftEffect getCurrentTime
--    let next = head $ starting now sched
--    if next == now then executeAndScheduleNext f sched now
--                   else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext f sched
--
--executeAndScheduleNext :: (Monad m) => (UTCTime -> m ()) -> Schedule Freq -> UTCTime -> m ()
--executeAndScheduleNext f sched now = do
--   f now
--   let rest = drop 1 $ starting now sched
--   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext f sched
--
--schedule_ :: (Monad m) => Schedule Freq -> m () -> m ()
--schedule_ ts f = schedule ts (const f)
--
----at each time provided, the supplied function will be called
--schedule' :: (Monad m) => [UTCTime] -> (UTCTime -> m ()) -> m ()
--schedule' sched f = do
--    let sched' = sort sched
--    now <- liftEffect getCurrentTime
--    let nextMay = headMay $ filter (>=now) sched'
--    case nextMay of
--        Just next -> if next == now then executeAndScheduleNext' f sched' now
--                                    else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext' f sched'
--        Nothing -> return ()
--
--
--executeAndScheduleNext' :: (Monad m) => (UTCTime -> m ()) -> [UTCTime] -> UTCTime -> m ()
--executeAndScheduleNext' f sched now = do
--   f now
--   let rest = drop 1 sched
--   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext' f sched
--
--
--schedule'_ :: (Monad m) => [UTCTime] -> m () -> m ()
--schedule'_ ts f = schedule' ts (const f)

-- * Individual events

-- | Build an event firing at a specific time
timeEvent :: UTCTime -> Event UTCTime
timeEvent = SignalEvent . Time

-- | Build a message event, that can be intercepted by another rule
-- this is useful for message-passing style of communication
messageEvent :: (Typeable a) => Msg a -> Event a
messageEvent = SignalEvent . Message

-- | Build a event firing immediatly, yelding the value of the NomexNE
--liftEvent :: NomexNE a -> Event a
--liftEvent = LiftEvent

-- | duration
oneWeek, oneDay, oneHour, oneMinute :: NominalDiffTime
oneWeek = 7 * oneDay
oneDay = 24 * oneHour
oneHour = 60 * oneMinute
oneMinute = 60

-- * internals

inputFormSignal :: (Typeable a) => PlayerNumber -> String -> (InputForm a) -> Signal a
inputFormSignal pn s iform = Input pn s iform

signalEvent :: (Typeable a) => Signal a -> Event a
signalEvent = SignalEvent
