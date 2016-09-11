{-# LANGUAGE ScopedTypeVariables #-}

module Language.Nomyx.Messages --(
  --sendMessage, sendMessage_,
  --onMessage, onMessageOnce,
  --APICall(..), onAPICall, callAPI, callAPIBlocking,
   where

import Language.Nomyx.Expression
import Imprevu.Variables
import Imprevu.Event
import Imprevu.Events
import Imprevu.Messages (APICall)
import qualified Imprevu.Messages as Imp
import Data.Typeable
import Control.Monad.Loops (untilJust)
import Control.Monad

-- * Messages
-- a rule can send a simple message to another rule, and subscribe to a message.

-- | broadcast a message that can be catched by another rule
sendMessage :: (Typeable a, Show a) => Msg a -> a -> Nomex ()
sendMessage = SendMessage

-- | send an empty message
sendMessage_ :: String -> Nomex ()
sendMessage_ = Imp.sendMessage_

-- | subscribe on a message
onMessage :: (Typeable m, Show m, Eq m) => Msg m -> (m -> Nomex ()) -> Nomex EventNumber
onMessage = Imp.onMessage

-- | subscribe on a message, delete it on the first call
onMessageOnce :: (Typeable m, Show m, Eq m) => Msg m -> (m -> Nomex ()) -> Nomex EventNumber
onMessageOnce = Imp.onMessageOnce

-- * API calls
-- Nomyx Rule can register an API function with 'onAPICall' to provide services to other rules.
-- other Rules are then able to call 'callAPI' or 'callAPIBlocking' to access the services.
-- API calls between Rules are build using message passing.

-- version with one parameters
onAPICall :: (Typeable a, Show a, Eq a, Typeable r, Show r, Eq r) => APICall a r -> (a -> Nomex r) -> Nomex EventNumber
onAPICall = Imp.onAPICall

-- | version with one parameters
callAPI :: (Typeable a, Show a, Eq a, Typeable r, Show r, Eq r) => APICall a r -> a -> (r -> Nomex ()) -> Nomex ()
callAPI = Imp.callAPI

-- | call an API function and wait for the result.
callAPIBlocking :: (Typeable a, Show a, Eq a, Typeable r, Show r, Eq r) => APICall a r -> a -> Nomex r
callAPIBlocking = Imp.callAPIBlocking

