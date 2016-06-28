
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

--data EventInfoIO = EventInfoIO { unEvents :: [EventInfo EventIO]}
newtype Events a = Events (Evaluate Events [String] a)
  deriving (Functor, Applicative, Monad)


instance EvMgt Events where
   onEvent         = evOnEvent
   delEvent     en = undefined --modify (filter (\a -> _eventNumber a /= en)) >> return True
   getEvents       = undefined
   sendMessage     = undefined -- :: (Typeable a, Show a) => Msg a -> a -> n ()
   currentTime     = undefined

evalEvents :: Events a -> Evaluate Events [String] a
evalEvents (Events es) = es
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

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Events ()) -> Events EventNumber
evOnEvent ev h = Events $ do
   evs <- get
   let en = getFreeNumber (map _eventNumber $ _events evs)
   modify (\(EvalEnv es s f g) -> (EvalEnv (EventInfo en ev h SActive [] : es) s f g))
   return en

test :: IO ()
test = do
  let (EvalEnv _ s _ _) = runIdentity $ flip execStateT (EvalEnv [] [] (evalEvents . void) undefined) $ do
      r <- runErrorT $ do
        void $ evalEvents testEvent
        triggerEvent (Signal True) "Hello"
        return ()
      case r of
        Right a -> return a
        Left e -> error $ show "error occured"
  putStrLn $ show s

testEvent :: Events ()
testEvent = do
   onEvent_ (SignalEvent (Signal True)) (putStrLn')
   return ()

putStrLn' :: String -> Events ()
putStrLn' s = Events $ do
  (EvalEnv is ss f g) <- get
  put (EvalEnv is (s:ss) f g)
