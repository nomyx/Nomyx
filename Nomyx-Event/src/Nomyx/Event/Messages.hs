{-# LANGUAGE ScopedTypeVariables #-}

module Language.Nomyx.Messages (
  sendMessage, sendMessage_,
  onMessage, onMessageOnce,
  APICall(..), onAPICall, callAPI, callAPIBlocking,  
  ) where

import Language.Nomyx.Expression
import Language.Nomyx.Events
import Language.Nomyx.Variables
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
sendMessage_ m = SendMessage (Msg m) ()

-- | subscribe on a message
onMessage :: (Typeable m, Show m) => Msg m -> (m -> Nomex ()) -> Nomex EventNumber
onMessage name = onEvent_ (messageEvent name)

-- | subscribe on a message, delete it on the first call
onMessageOnce :: (Typeable m, Show m) => Msg m -> (m -> Nomex ()) -> Nomex EventNumber
onMessageOnce name = onEventOnce (messageEvent name)

-- * API calls
-- Nomyx Rule can register an API function with 'onAPICall' to provide services to other rules.
-- other Rules are then able to call 'callAPI' or 'callAPIBlocking' to access the services.
-- API calls between Rules are build using message passing.

-- | types of API calls
data APICall a r = APICall String

-- version with one parameters
onAPICall :: (Typeable a, Show a, Typeable r, Show r) => APICall a r -> (a -> Nomex r) -> Nomex EventNumber
onAPICall (APICall name) action = onMessage (Msg name) (\(msg, a) -> action a >>= sendMessage msg)

-- | version with one parameters
callAPI :: (Typeable a, Show a, Typeable r, Show r) => APICall a r -> a -> (r -> Nomex ()) -> Nomex ()
callAPI (APICall name) a callback = do
   msgTemp <- createTempMsg callback
   sendMessage (Msg name) (msgTemp, a)

-- | call an API function and wait for the result.
callAPIBlocking :: (Typeable a, Show a, Typeable r, Show r) => APICall a r -> a -> Nomex r
callAPIBlocking apiName param = do
  v <- getTempVar Nothing
  callAPI apiName param (\r -> void $ writeVar v (Just r))
  r <- untilJust $ readVar_ v
  delVar v
  return r

-- * Internals

-- | creates a temporary message with a random name
createTempMsg :: (Typeable m, Show m) => (m -> Nomex ()) -> Nomex (Msg m)
createTempMsg callback = do
  r <- getRandomNumber (0, 100000::Int)
  let msg = Msg ("APIcallback" ++ (show r))
  onMessageOnce msg callback
  return msg
