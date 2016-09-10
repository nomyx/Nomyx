{-# LANGUAGE ScopedTypeVariables #-}

-- | All the building blocks to allow rules to build events.
module Language.Nomyx.Events (
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

import Language.Nomyx.Expression
import Data.Typeable
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Safe


-- * Events

-- | register a callback on an event
onEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> Nomex EventNumber
onEvent = OnEvent

-- | register a callback on an event, disregard the event number
onEvent_ :: (Typeable e, Show e) => Event e -> (e -> Nomex ()) -> Nomex EventNumber
onEvent_ e h = onEvent e (\(_, d) -> h d)

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable e, Show e) => Event e -> (e -> Nomex ()) -> Nomex EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent en >> h ed
    OnEvent e handler

delEvent :: EventNumber -> Nomex Bool
delEvent = DelEvent

getEvents :: Nomex [EventInfo]
getEvents = GetEvents

getEvent :: EventNumber -> Nomex (Maybe EventInfo)
getEvent en = find (\(EventInfo en2 _ _ _ evst _) -> en == en2 && evst == SActive) <$> getEvents

getIntermediateResults :: EventNumber -> Nomex (Maybe [(PlayerNumber, SomeData)])
getIntermediateResults en = do
   mev <- getEvent en
   case mev of
      Just ev -> return $ Just $ mapMaybe getInputResult (_env ev)
      Nothing -> return Nothing

getInputResult :: SignalOccurence -> Maybe (PlayerNumber, SomeData)
getInputResult (SignalOccurence (SignalData (Input pn _ _) r) _) = Just (pn, SomeData r)
getInputResult _ = Nothing

-- | on the provided schedule, the supplied function will be called
schedule :: Schedule Freq -> (UTCTime -> Nomex ()) -> Nomex ()
schedule sched f = do
    now <- getCurrentTime
    let next = head $ starting now sched
    if next == now then executeAndScheduleNext f sched now
                   else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext f sched

executeAndScheduleNext :: (UTCTime -> Nomex ()) -> Schedule Freq -> UTCTime -> Nomex ()
executeAndScheduleNext f sched now = do
   f now
   let rest = drop 1 $ starting now sched
   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext f sched

schedule_ :: Schedule Freq -> Nomex () -> Nomex ()
schedule_ ts f = schedule ts (const f)

--at each time provided, the supplied function will be called
schedule' :: [UTCTime] -> (UTCTime -> Nomex ()) -> Nomex ()
schedule' sched f = do
    let sched' = sort sched
    now <- getCurrentTime
    let nextMay = headMay $ filter (>=now) sched'
    case nextMay of
        Just next -> if next == now then executeAndScheduleNext' f sched' now
                                    else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext' f sched'
        Nothing -> return ()


executeAndScheduleNext' :: (UTCTime -> Nomex ()) -> [UTCTime] -> UTCTime -> Nomex ()
executeAndScheduleNext' f sched now = do
   f now
   let rest = drop 1 sched
   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext' f sched


schedule'_ :: [UTCTime] -> Nomex () -> Nomex ()
schedule'_ ts f = schedule' ts (const f)

-- | get the current time as UTCTime
getCurrentTime :: Nomex UTCTime
getCurrentTime = CurrentTime

-- * Individual events

-- | Build an event firing at a specific time
timeEvent :: UTCTime -> Event UTCTime
timeEvent = SignalEvent . Time

-- | Build a event firing when the victory condition is changed
victoryEvent :: Event VictoryInfo
victoryEvent = SignalEvent Victory

-- | Build a event firing when a player arrives or leaves
playerEvent :: Player -> Event PlayerInfo
playerEvent = SignalEvent . Player

-- | Build a event firing when an action is made on a rule
ruleEvent :: RuleEvent -> Event RuleInfo
ruleEvent re = SignalEvent $ RuleEv re

-- | Build a message event, that can be intercepted by another rule
-- this is useful for message-passing style of communication
messageEvent :: (Typeable a) => Msg a -> Event a
messageEvent = SignalEvent . Message

-- | Build a event firing immediatly, yelding the value of the NomexNE
liftEvent :: Nomex a -> Event a
liftEvent = LiftEvent

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
