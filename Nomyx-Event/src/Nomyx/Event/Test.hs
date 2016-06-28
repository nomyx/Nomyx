
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Nomyx.Event.Test where

import Nomyx.Event.Events
import Nomyx.Event.Events2
import Nomyx.Event.EvMgt
import Nomyx.Event.Internal.EventEval
import Nomyx.Event.Internal.Utils
import Control.Monad.State
import Control.Monad.Error
import Data.Typeable
import Control.Lens
import Data.Time

data EventInfoIO = EventInfoIO { unEvents :: [EventInfo EventIO]}
type EventIO = StateT EventInfoIO IO

instance EvMgt EventIO where
   onEvent         = evOnEvent
   delEvent     en = undefined --modify (filter (\a -> _eventNumber a /= en)) >> return True
   getEvents       = do
     eii <- get  --StateT [EventInfo IO] IO [EventInfo ]
     return $ unEvents eii
   sendMessage     = undefined -- :: (Typeable a, Show a) => Msg a -> a -> n ()
   currentTime     = liftIO getCurrentTime

evalEventIO :: EventIO a -> Evaluate EventIO () a
evalEventIO eio = do
  (EvalEnv eis s f g) <- get
  (a, eis') <- runStateT eio (EventInfoIO eis)
  put (EvalEnv eis' s f g)
  return a
--  withStateT (\(EventInfoIO eis) -> (EvalEnv eis () evalEventIO undefined)) eio


--class (Typeable n, Monad n, Applicative n) => EvMgt n where
--   --Events management
--   onEvent         :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> n ()) -> n EventNumber
--   delEvent        :: EventNumber -> n Bool
--   getEvents       :: StateT [EventInfo IO] IO [EventInfo n]
--   sendMessage     :: (Typeable a, Show a) => Msg a -> a -> n ()
--   currentTime     :: n UTCTime
--
--get :: Monad m => StateT s m s

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> EventIO ()) -> EventIO EventNumber
evOnEvent ev h = do
   evs <- get
   let en = getFreeNumber (map _eventNumber $ unEvents evs)
   modify (\(EventInfoIO es) -> EventInfoIO $ EventInfo en ev h SActive [] : es)
   return en

test :: IO ()
test = do
  flip evalStateT (EvalEnv [] () (evalEventIO . void) undefined) $ do
    r <- runErrorT $ do
      evalEventIO testEvent
      triggerEvent (Signal True) "Hello"
      return ()
    case r of
      Right a -> return a
      Left e -> error $ show e

testEvent :: EventIO ()
testEvent = do
   onEvent_ (SignalEvent (Signal True)) (liftIO . putStrLn)
   return ()

