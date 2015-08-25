
module Language.Nomyx.Messages (
   sendMessage, sendMessage_,
   onMessage, onMessageOnce,
   onAPICall, callAPI, APIName
   ) where

import Language.Nomyx.Expression
import Language.Nomyx.Events
import Data.Typeable

-- * Messages
-- a rule can send a simple message to another rule, and subscribe to a message.
-- under the hood, a Message is just a regular type of event.

-- | broadcast a message that can be catched by another rule
sendMessage :: (Typeable a, Show a) => Msg a -> a -> Nomex ()
sendMessage = SendMessage

sendMessage_ :: String -> Nomex ()
sendMessage_ m = SendMessage (Msg m) ()

-- | subscribe on a message
onMessage :: (Typeable m, Show m) => Msg m -> (m -> Nomex ()) -> Nomex EventNumber
onMessage name = onEvent_ (messageEvent name)

onMessageOnce :: (Typeable m, Show m) => Msg m -> (m -> Nomex ()) -> Nomex EventNumber
onMessageOnce name = onEventOnce (messageEvent name)

-- * API calls
-- Nomyx Rule can register an API function with 'onAPICall' to provide services to other rules.
-- other Rules are then able to call 'callAPI' or 'callAPIBlocking' to access the services.
-- API calls between Rules are build using message passing.

-- | alias for a API function name
type APIName = String

-- | subscribe an API call with a function name and a value to return
-- the value in toSend will be sent back to the caller.
onAPICall :: (Typeable a, Show a) => APIName -> Nomex a -> Nomex EventNumber
onAPICall name toSend = onMessage (Msg name) ((toSend >>=) . sendMessage)

-- | call an API function. The result of the call will be passed to the callback when available.
callAPI :: (Typeable a, Show a) => APIName -> (a -> Nomex ()) -> Nomex ()
callAPI name callback = do
   msgTemp <- createTempMsg callback
   sendMessage (Msg name) msgTemp


-- * Internals

-- | creates a temporary message with a random name
createTempMsg :: (Typeable m, Show m) => (m -> Nomex ()) -> Nomex (Msg m)
createTempMsg callback = do
  r <- getRandomNumber (0, 100000::Int)
  let msg = Msg ("APIcallback" ++ (show r))
  onMessageOnce msg callback
  return msg
