{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | All the building blocks to allow rules to build events.
module Nomyx.Language.Events (
   onEvent, onEvent_, onEventOnce,
   delEvent,
   getEvents,
   getIntermediateResults,
   schedule, schedule_, schedule', --schedule'_,
   getCurrentTime,
   oneWeek, oneDay, oneHour, oneMinute,
   --timeEvent, messageEvent, victoryEvent, playerEvent, ruleEvent,
   --signalEvent, inputFormSignal,
   liftEvent,
   victoryEvent, timeEvent,
   Imprevu.EventNumber,
   SomeData,
   Victory(..)
   ) where

import           Nomyx.Language.Types
import           Imprevu (EventNumber)
import qualified Imprevu as Imp
import           Data.Typeable
import           Data.Time hiding (getCurrentTime)
import           Data.Time.Recurrence hiding (filter)


-- * Events

-- | register a callback on an event
onEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> Nomex EventNumber
onEvent = Imp.onEvent

-- | register a callback on an event, disregard the event number
onEvent_ :: (Typeable e, Show e) => Event e -> (e -> Nomex ()) -> Nomex EventNumber
onEvent_ = Imp.onEvent_

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable e, Show e) => Event e -> (e -> Nomex ()) -> Nomex EventNumber
onEventOnce = Imp.onEventOnce

delEvent :: EventNumber -> Nomex Bool
delEvent = Imp.delEvent

getEvents :: Nomex [EventInfo]
getEvents = Imp.getEvents

getIntermediateResults :: EventNumber -> Nomex (Maybe [(PlayerNumber, SomeData)])
getIntermediateResults = Imp.getIntermediateResults

-- | on the provided schedule, the supplied function will be called
schedule :: Schedule Freq -> (UTCTime -> Nomex ()) -> Nomex ()
schedule = Imp.schedule

schedule_ :: Schedule Freq -> Nomex () -> Nomex ()
schedule_ = Imp.schedule_

--at each time provided, the supplied function will be called
schedule' :: [UTCTime] -> (UTCTime -> Nomex ()) -> Nomex ()
schedule' = Imp.schedule'

--schedule'_ :: [UTCTime] -> Nomex () -> Nomex ()
--schedule'_ ts f = schedule' ts (const f)

-- | get the current time as UTCTime
getCurrentTime :: Nomex UTCTime
getCurrentTime = GetCurrentTime

-- * Individual events

-- | Build an event firing at a specific time
timeEvent :: UTCTime -> Event UTCTime
timeEvent = Imp.timeEvent

data Victory = Victory
  deriving (Eq, Show)

-- | Build a event firing when the victory condition is changed
victoryEvent :: Event VictoryInfo
victoryEvent = SignalEvent $ Signal Victory

-- | Build a event firing immediatly, yelding the value of the NomexNE
liftEvent :: Nomex a -> Event a
liftEvent = Imp.liftEvent

-- | duration
oneWeek, oneDay, oneHour, oneMinute :: NominalDiffTime
oneWeek = 7 * oneDay
oneDay = 24 * oneHour
oneHour = 60 * oneMinute
oneMinute = 60



