{-# LANGUAGE ScopedTypeVariables #-}

-- | All the building blocks to allow rules to build events.
module Imprevu.Events2
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

import Imprevu.Events
import Imprevu.EvMgt
import Data.Typeable
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Safe


--Input   :: PlayerNumber -> String -> (InputForm a) -> Signal a  -- Fires when the user has complete the input form. Input forms are created automatically when the event is posted.
--Player  :: Player    -> Signal PlayerInfo                       -- Fires on events related to players.
--RuleEv  :: RuleEvent -> Signal RuleInfo                         -- Fires on events related to rules.
--Time    :: UTCTime   -> Signal UTCTime                          -- Fires at the specified date.
--data Message = Msg a     -> Signal a                                -- Fires if a rule sends a message.
--Victory ::              Signal VictoryInfo                      -- Fires if the victory condition is changed.

-- * Events

-- | register a callback on an event, disregard the event number
onEvent_ :: (Typeable a, Show a, EvMgt n) => Event a -> (a -> n ()) -> n EventNumber
onEvent_ e h = onEvent e (\(_, d) -> h d)

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable a, Show a, EvMgt n) => Event a -> (a -> n ()) -> n EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent en >> h ed
    onEvent e handler

--delEvent :: EventNumber -> Nomex Bool
--delEvent = DelEvent

--getEvents :: Nomex [EventInfo]
--getEvents = GetEvents

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
messageEvent :: (Typeable a) => Msg a -> Event a
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

signalEvent :: (Signal e) => e -> Event (SignalDataType e)                -- Embed a single Signal as an Event
signalEvent = SignalEvent
