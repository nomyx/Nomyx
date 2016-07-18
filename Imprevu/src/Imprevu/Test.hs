
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Imprevu.Test where

import Imprevu.Events
import Imprevu.Events2
import Imprevu.Inputs
import Imprevu.Messages
import Imprevu.EvMgt
import Imprevu.Internal.EventEval
import Imprevu.Internal.Utils
import Control.Monad.State
import Control.Monad.Error
import Data.Typeable
import Control.Lens
import Data.Time
import Control.Concurrent.STM
import System.IO.Unsafe
import System.Random

--type Evaluate n s a = ErrorT String (State (EvalEnv n s)) a
data TestState = TestState {eventInfos :: [EventInfo TestIO],
                            outputs :: [String]}

newtype TestIO a = TestIO {unTestIO :: StateT TestState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TestState)

instance MonadError String TestIO where
  throwError = undefined
  catchError = undefined

instance EvMgt TestIO where
   onEvent         = evOnEvent
   delEvent     en = undefined --modify (filter (\a -> _eventNumber a /= en)) >> return True
   getEvents       = error "not implem"
   sendMessage m a = eventsEval $ triggerEvent m a -- :: (Typeable a, Show a) => Msg a -> a -> n ()
   currentTime     = liftIO getCurrentTime
   getRandomNumber a = liftIO $ randomRIO a

eventsEval :: Evaluate TestIO [String] () -> TestIO ()
eventsEval eval = do
   (TestState eis os) <- get
   let (EvalEnv eis' os' f g) = runIdentity $ flip execStateT (EvalEnv eis os (void . evalEvents) undefined) $ do
       res <- runErrorT eval
       case res of
         Right a -> return a
         Left e -> error $ show "error occured"
   put (TestState eis' os')

evalEvents :: TestIO a -> Evaluate TestIO [String] a
evalEvents (TestIO tio) = do
   (EvalEnv (eis :: [EventInfo TestIO]) (s :: [String]) f g) <- get
   let (a, (TestState eis' s')) = unsafePerformIO $ runStateT tio (TestState eis s)
   put (EvalEnv eis' s' f g)
   return a

--data EvalEnv n s = EvalEnv { _events      :: [EventInfo n],
--                             _execState   :: s,
--                             evalFunc :: forall a. (Show a) => n a -> Evaluate n s (),       -- evaluation function
--                             errorHandler :: EventNumber -> String -> Evaluate n s ()}    -- error function
--type Evaluate n s a = ErrorT String (State (EvalEnv n s)) a

--class (Typeable n, Monad n, Applicative n) => EvMgt n where
--   --Events management
--   onEvent         :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> n ()) -> n EventNumber
--   delEvent        :: EventNumber -> n Bool
--   getEvents       :: StateT [EventInfo IO] IO [EventInfo n]
--   sendMessage     :: (Typeable a, Show a) => Msg a -> a -> n ()
--   currentTime     :: n UTCTime
--
--get :: Monad m => StateT s m s

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> TestIO ()) -> TestIO EventNumber
evOnEvent ev h = do
   (TestState evs os) <- get
   let en = getFreeNumber (map _eventNumber evs)
   put (TestState ((EventInfo en ev h SActive []) : evs) os)
   return en

--evSendMessage :: (Typeable a, Show a) => Msg a -> a -> Evaluate ()
--evSendMessage m = triggerEvent (Message m)

execEvents :: (Signal e) => TestIO a -> e -> SignalDataType e -> [String]
execEvents r f d = _execState $ runIdentity $ flip execStateT (EvalEnv [] [] (void . evalEvents) undefined) $ do
   res <- runErrorT $ do
      void $ evalEvents r
      triggerEvent f d
      return ()
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

exec :: TestIO a -> [String]
exec r = _execState $ runIdentity $ flip execStateT (EvalEnv [] [] (void . evalEvents) undefined) $ do
   res <- runErrorT $ do
      void $ evalEvents r
      return ()
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

putStrLn' :: String -> TestIO ()
putStrLn' s = do
  (TestState is ss) <- get
  put (TestState is (s:ss))


data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: TestIO ()
testSingleInput = void $ onInputRadio_ "Vote for Holland or Sarkozy" [Holland, Sarkozy] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

testSingleInputEx :: Bool
testSingleInputEx = "voted for Holland" `elem` g where
   g = execEvents testSingleInput (Input "Vote for Holland or Sarkozy" (Radio [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) Holland

testMultipleInputs :: TestIO ()
testMultipleInputs = void $ onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

testMultipleInputsEx :: Bool
testMultipleInputsEx = "voted for [Holland,Sarkozy]" `elem` g where
   g = execEvents testMultipleInputs (Input "Vote for Holland and Sarkozy" (Checkbox [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) [Holland, Sarkozy]

testInputString :: TestIO ()
testInputString = void $ onInputText_ "Enter a number:" h 1 where
   h a = putStrLn' ("You entered: " ++ a)

testInputStringEx :: Bool
testInputStringEx = "You entered: 1" `elem` g where
   g = execEvents testInputString (Input "Enter a number:" Text) "1"

-- Test message
testSendMessage :: TestIO ()
testSendMessage = do
    let msg = Msg "msg" :: Msg String
    onEvent_ (messageEvent msg) f
    sendMessage msg "toto" where
        f (a :: String) = putStrLn' a

testSendMessageEx :: Bool
testSendMessageEx = "toto" `elem` (exec testSendMessage)

testSendMessage2 :: TestIO ()
testSendMessage2 = do
    onEvent_ (messageEvent (Msg "msg" :: Msg ())) $ const $ putStrLn' "Received"
    sendMessage_ "msg"

testSendMessageEx2 :: Bool
testSendMessageEx2 = "Received" `elem` (exec testSendMessage2)

testAPICall :: TestIO ()
testAPICall = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    callAPI call "toto" putStrLn'

testAPICallEx :: Bool
testAPICallEx = "toto" `elem` (exec testAPICall)

testAPICall2 :: TestIO ()
testAPICall2 = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    a <- callAPIBlocking call "toto"
    putStrLn' a

testAPICallEx2 :: Bool
testAPICallEx2 = "toto" `elem` (exec testAPICall2)
