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
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}

-- | This module contains the type definitions necessary to build a Nomic rule.
module Imprevu.Types where

import           Control.Applicative hiding (Const)
import           Control.Lens
import           Control.Shortcut
import           Control.Monad
import           Data.Typeable
import           GHC.Generics

-- * Event

-- | Composable events
data EventM n a where
   SumEvent       :: EventM n a -> EventM n a -> EventM n a                        -- The first event to fire will be returned
   AppEvent       :: EventM n (a -> b) -> EventM n a -> EventM n b                 -- Both events should fire, and then the result is returned
   PureEvent      :: a -> EventM n a                                         -- Create a fake event. The result is useable with no delay.
   EmptyEvent     :: EventM n a                                              -- An event that is never fired.
   BindEvent      :: EventM n a -> (a -> EventM n b) -> EventM n b                 -- A First event should fire, then a second event is constructed
   ShortcutEvents :: [EventM n a] -> ([Maybe a] -> Bool) -> EventM n [Maybe a]  -- Return the intermediate results as soon as the function evaluates to True, dismissing the events that hasn't fired yet
   SignalEvent    :: (Eq s, Typeable s, Show s, Typeable a, Show a) => Signal s a -> EventM n a                                  -- Embed a single Signal as an EventM
   LiftEvent      :: n a -> EventM n a                                       -- create an event containing the result of the monad evaluation
   deriving Typeable

instance Functor (EventM n) where
   fmap f a = pure f <*> a

instance Applicative (EventM n) where
   pure = PureEvent
   (<*>) = AppEvent

instance Alternative (EventM n) where
   (<|>) = SumEvent
   empty = EmptyEvent

instance Monad (EventM n) where
   (>>=) = BindEvent
   return = PureEvent

instance MonadPlus (EventM n) where
   mplus = SumEvent
   mzero = EmptyEvent

instance Shortcutable (EventM n) where
   shortcut = ShortcutEvents

type ClientNumber = Int

data Signal s a where
  Signal :: s -> Signal s a

deriving instance (Show s, Show a) => Show (Signal s a)
deriving instance (Eq s) => Eq (Signal s a)
deriving instance (Typeable s, Typeable a) => Typeable (Signal s a)

data InputS a where
  InputS :: Input a -> ClientNumber -> InputS a

deriving instance (Eq a) => Eq (InputS a)
deriving instance (Show a) => Show (InputS a)

-- | Input forms as programmed by the user
data Input a where
   Text     :: String ->             Input String
   TextArea :: String ->             Input String
   Button   :: String ->             Input ()
   Radio    :: String -> [(Int, String)] -> Input Int
   Checkbox :: String -> [(Int, String)] -> Input [Int]
   deriving (Typeable)

deriving instance Show (Input a)
deriving instance Eq (Input a)

-- | Type agnostic base signal
data SomeSignal = forall a s. (Typeable a, Typeable s, Show a, Show s) => SomeSignal (Signal s a)

deriving instance Show SomeSignal

-- | Type agnostic result data
data SomeData = forall e. (Typeable e, Show e) => SomeData e

deriving instance Show SomeData



-- * EventInfoN

type EventNumber = Int
type EventName = String

-- EventInfoN holds all infos on an active event
data EventInfoN n = forall a. (Typeable a, Show a) =>
   EventInfo {_eventNumber :: EventNumber,
              event        :: EventM n a,
              handler      :: (EventNumber, a) -> n (),
              _evStatus    :: Status,
              _env         :: [SignalOccurence]}

instance Eq (EventInfoN n) where
   (EventInfo {_eventNumber=e1}) == (EventInfo {_eventNumber=e2}) = e1 == e2

instance Ord (EventInfoN n) where
   (EventInfo {_eventNumber=e1}) <= (EventInfo {_eventNumber=e2}) = e1 <= e2

instance Show (EventInfoN n) where
   show (EventInfo en _ _ s envi) =
      "event num: " ++ (show en) ++
      ", envs: " ++ (show envi) ++
      ", status: " ++ (show s)

eventNumber :: Lens' (EventInfoN n) EventNumber
eventNumber f (EventInfo e ev h evs env) = fmap (\e' -> (EventInfo e' ev h evs env)) (f e)

evStatus :: Lens' (EventInfoN n) Status
evStatus f (EventInfo e ev h evs env) = fmap (\evs' -> (EventInfo e ev h evs' env)) (f evs)

env :: Lens' (EventInfoN n) [SignalOccurence]
env f (EventInfo e ev h evs env) = fmap (\env' -> (EventInfo e ev h evs env')) (f env)

-- status of an event
data Status = SActive | SDeleted deriving (Eq, Show)

-- data and addres from an occurence of a signal
data SignalOccurence = SignalOccurence {_signalOccData    :: SignalData,
                                        _signalOccAddress :: SignalAddress}

deriving instance Show SignalOccurence

-- result data from a signal
data SignalData = forall s a. (Typeable s, Typeable a, Show s, Show a, Eq s) =>
   SignalData {signal     :: Signal s a,
               signalData :: a}

instance Show SignalData where
  show (SignalData s sd) = show s ++ " " ++ (show sd)

-- SignalAddress is a representation of the address of a signal in the event tree
type SignalAddress = [SignalAddressElem]
data SignalAddressElem = SumR | SumL | AppR | AppL | BindR | BindL | Shortcut deriving (Show, Read, Ord, Eq, Generic)

makeLenses ''SignalOccurence

