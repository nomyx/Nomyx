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
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE FlexibleContexts                #-}

-- | This module contains the type definitions necessary to build a Nomic rule.
module Imprevu.Internal.Event where

import           Control.Applicative hiding (Const)
import           Control.Lens
import           Control.Monad.Error
import           Control.Shortcut
import           Data.Data           (Data)
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           System.Random
import           Imprevu.Internal.Utils

-- * Event

-- | Composable events
data Event a where
   SumEvent       :: Event a -> Event a -> Event a                        -- The first event to fire will be returned
   AppEvent       :: Event (a -> b) -> Event a -> Event b                 -- Both events should fire, and then the result is returned
   PureEvent      :: a -> Event a                                         -- Create a fake event. The result is useable with no delay.
   EmptyEvent     :: Event a                                              -- An event that is never fired.
   BindEvent      :: Event a -> (a -> Event b) -> Event b                 -- A First event should fire, then a second event is constructed
   ShortcutEvents :: [Event a] -> ([Maybe a] -> Bool) -> Event [Maybe a]  -- Return the intermediate results as soon as the function evaluates to True, dismissing the events that hasn't fired yet
   SignalEvent    :: (Signal e) => e -> Event (SignalDataType e)                -- Embed a single Signal as an Event
   deriving Typeable

instance Functor (Event) where
   fmap f a = pure f <*> a

instance Applicative (Event) where
   pure = PureEvent
   (<*>) = AppEvent

instance Alternative (Event) where
   (<|>) = SumEvent
   empty = EmptyEvent

instance Monad (Event) where
   (>>=) = BindEvent
   return = PureEvent

instance MonadPlus (Event) where
   mplus = SumEvent
   mzero = EmptyEvent

instance Shortcutable (Event) where
   shortcut = ShortcutEvents

-- | Signals
-- A signal is something that may occur at a point in time.
-- They are the leafs of the event tree
class (Eq e, Typeable e, Show e, Data e, Typeable (SignalDataType e)) => Signal e where
  type SignalDataType e :: *

-- | Type agnostic base signal
data SomeSignal = forall a. (Signal a, Typeable a, Data a) => SomeSignal a

--deriving instance (Show e) => Show (SomeSignal e)

-- | Type agnostic result data
data SomeData = forall e. (Typeable e, Show e) => SomeData e
deriving instance Show SomeData

instance Eq SomeSignal where
   (SomeSignal e1) == (SomeSignal e2) = e1 === e2

-- * EventInfo

type EventNumber = Int
type EventName = String

-- EventInfo holds all infos on a active event
data EventInfo n = forall a. (Typeable a, Show a) =>
   EventInfo {_eventNumber :: EventNumber,
              event        :: Event a,
              handler      :: (EventNumber, a) -> n (),
              _evStatus    :: Status,
              _env         :: [SignalOccurence]}

instance Eq (EventInfo n) where
   (EventInfo {_eventNumber=e1}) == (EventInfo {_eventNumber=e2}) = e1 == e2

instance Ord (EventInfo n) where
   (EventInfo {_eventNumber=e1}) <= (EventInfo {_eventNumber=e2}) = e1 <= e2

eventNumber :: Lens' (EventInfo n) EventNumber
eventNumber f (EventInfo e ev h evs env) = fmap (\e' -> (EventInfo e' ev h evs env)) (f e)

evStatusNumber :: Lens' (EventInfo n) Status
evStatusNumber f (EventInfo e ev h evs env) = fmap (\evs' -> (EventInfo e ev h evs' env)) (f evs)

env :: Lens' (EventInfo n) [SignalOccurence]
env f (EventInfo e ev h evs env) = fmap (\env' -> (EventInfo e ev h evs env')) (f env)

-- status of an event
data Status = SActive | SDeleted deriving (Eq, Show)

-- data and addres from an occurence of a signal
data SignalOccurence = SignalOccurence {_signalOccData    :: SignalData,
                                        _signalOccAddress :: SignalAddress}

-- result data from a signal
data SignalData = forall a. (Signal a, Show a) =>
   SignalData {signal     :: a,
               signalData :: SignalDataType a}


-- SignalAddress is a representation of the address of a signal in the event tree
type SignalAddress = [SignalAddressElem]
data SignalAddressElem = SumR | SumL | AppR | AppL | BindR | BindL | Shortcut deriving (Show, Read, Ord, Eq, Generic)

makeLenses ''SignalOccurence

