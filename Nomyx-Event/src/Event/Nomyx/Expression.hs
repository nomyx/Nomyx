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

-- | This module contains the type definitions necessary to build a Nomic rule.
module Event.Nomyx.Expression where

import           Control.Applicative hiding (Const)
import           Control.Lens
import           Control.Monad.Error
import           Control.Shortcut
import           Data.Data           (Data)
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           System.Random

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int
type RuleName = String
type RuleDesc = String
type RuleText = String
type RuleCode = String
type EventNumber = Int
type EventName = String
type VarName = String
type OutputNumber = Int
type InputNumber = Int


data Nomex a

-- * Events

-- | Composable events
data Event a where
   SumEvent       :: Event a -> Event a -> Event a                        -- The first event to fire will be returned
   AppEvent       :: Event (a -> b) -> Event a -> Event b                 -- Both events should fire, and then the result is returned
   PureEvent      :: a -> Event a                                         -- Create a fake event. The result is useable with no delay.
   EmptyEvent     :: Event a                                              -- An event that is never fired.
   BindEvent      :: Event a -> (a -> Event b) -> Event b                 -- A First event should fire, then a second event is constructed
   ShortcutEvents :: [Event a] -> ([Maybe a] -> Bool) -> Event [Maybe a]  -- Return the intermediate results as soon as the function evaluates to True, dismissing the events that hasn't fired yet
   SignalEvent    :: (Typeable a) => Signal a -> Event a                  -- Embed a single Signal as an Event
--   LiftEvent      :: NomexNE a -> Event a                                 -- create an event containing the result of the NomexNE.
   deriving Typeable

-- | Signals
-- A signal is something that may occur at a point in time.
-- They are the leafs of the event tree
data Signal a where
   --Player  :: Player    -> Signal PlayerInfo                       -- Fires on events related to players.
   Input   :: String -> (InputForm a) -> Signal a  -- Fires when the user has complete the input form. Input forms are created automatically when the event is posted.
   Time    :: UTCTime   -> Signal UTCTime                          -- Fires at the specified date.
   Message :: Msg a     -> Signal a                                -- Fires if a message is received.
   --Custom  :: a         -> Signal b                                --
   deriving Typeable

-- | Type agnostic base signal
data SomeSignal = forall a. (Typeable a) => SomeSignal (Signal a)

-- | Type agnostic result data
data SomeData = forall e. (Typeable e, Show e) => SomeData e
deriving instance Show SomeData

-- | Events parameters
data Msg m     = Msg String deriving (Typeable, Show)

-- | Input forms
data InputForm a where
   Text     ::                                    InputForm String
   TextArea ::                                    InputForm String
   Button   ::                                    InputForm ()
   Radio    :: (Show a, Eq a) => [(a, String)] -> InputForm a
   Checkbox :: (Show a, Eq a) => [(a, String)] -> InputForm [a]
   deriving Typeable

deriving instance Show (InputForm a)
deriving instance Show (Signal a)
deriving instance Show SomeSignal
deriving instance Eq (Signal e)
deriving instance Eq (InputForm e)
deriving instance Eq (Msg e)

instance Functor Event where
   fmap f a = pure f <*> a

instance Applicative Event where
   pure = PureEvent
   (<*>) = AppEvent

instance Alternative Event where
   (<|>) = SumEvent
   empty = EmptyEvent

instance Monad Event where
   (>>=) = BindEvent
   return = PureEvent

instance MonadPlus Event where
   mplus = SumEvent
   mzero = EmptyEvent

instance Shortcutable Event where
   shortcut = ShortcutEvents


-- * EventInfo

-- EventInfo holds all infos on a active event
data EventInfo = forall e. (Typeable e, Show e) =>
   EventInfo {_eventNumber :: EventNumber,
              event        :: Event e,
              handler      :: EventHandler e,
              _evStatus    :: Status,
              _env         :: [SignalOccurence]}


-- SignalAddress is a representation of the address of a signal in the event tree
type SignalAddress = [SignalAddressElem]
data SignalAddressElem = SumR | SumL | AppR | AppL | BindR | BindL | Shortcut deriving (Show, Read, Ord, Eq, Generic)

-- result data from a signal
data SignalData = forall e. (Typeable e, Show e) =>
   SignalData {signal     :: Signal e,
               signalData :: e}

-- data and addres from an occurence of a signal
data SignalOccurence = SignalOccurence {_signalOccData    :: SignalData,
                                        _signalOccAddress :: SignalAddress}

-- function capable of handling the result of an event
type EventHandler e = (EventNumber, e) -> Nomex ()

deriving instance Show SignalData
deriving instance Show SignalOccurence

-- status of an event
data Status = SActive | SDeleted deriving (Eq, Show)

instance Eq EventInfo where
   (EventInfo {_eventNumber=e1}) == (EventInfo {_eventNumber=e2}) = e1 == e2

instance Ord EventInfo where
   (EventInfo {_eventNumber=e1}) <= (EventInfo {_eventNumber=e2}) = e1 <= e2


makeLenses ''SignalOccurence

eventNumber :: Lens' EventInfo EventNumber
eventNumber f (EventInfo e ev h evs env) = fmap (\e' -> (EventInfo e' ev h evs env)) (f e)

evStatusNumber :: Lens' EventInfo Status
evStatusNumber f (EventInfo e ev h evs env) = fmap (\evs' -> (EventInfo e ev h evs' env)) (f evs)

env :: Lens' EventInfo [SignalOccurence]
env f (EventInfo e ev h evs env) = fmap (\env' -> (EventInfo e ev h evs env')) (f env)

--eventNumber :: Lens' EventInfo EventNumber
--eventNumber f (EventInfo e rn ev h evs env) = fmap (\e' -> (EventInfo e' rn ev h evs env)) (f e)

--evStatusNumber :: Lens' EventInfo Status
--evStatusNumber f (EventInfo e rn ev h evs env) = fmap (\evs' -> (EventInfo e rn ev h evs' env)) (f evs)

--env :: Lens' EventInfo [SignalOccurence]
--env f (EventInfo e rn ev h evs env) = fmap (\env' -> (EventInfo e rn ev h evs env')) (f env)
