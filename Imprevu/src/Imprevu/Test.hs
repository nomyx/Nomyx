
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imprevu.Test where

import Imprevu.Events
import Imprevu.Inputs
import Imprevu.SysMgt
import Imprevu.Variables
import Imprevu.Messages
import Imprevu.Internal.Event
import Imprevu.Internal.EventEval
import Imprevu.Internal.Utils
import Control.Monad.State
import Control.Monad.Error
import Data.Typeable
import Data.List
import Data.Data
import Control.Lens
import Data.Time
import Control.Concurrent.STM
import System.IO.Unsafe
import System.Random

--type Evaluate n s a = ErrorT String (State (EvalEnv n s)) a
data TestState = TestState {eventInfos :: [EventInfo TestIO],
                            outputs    :: [String],
                            variable  :: Var}

newtype TestIO a = TestIO {unTestIO :: StateT TestState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TestState)

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a) =>
   Var { vName :: String,
         vData :: a}

instance MonadError String TestIO where
  throwError = undefined
  catchError = undefined

instance EvMgt TestIO where
   onEvent         = evOnEvent
   delEvent        = evDelEvent
   getEvents       = error "not implem"
   sendMessage m a = eventsEval $ triggerEvent m a

instance SysMgt TestIO where
   currentTime     = liftIO getCurrentTime
   getRandomNumber a = liftIO $ randomRIO a

instance VarMgt TestIO where
   newVar       v a = modify (\(TestState eis os _) -> (TestState eis os (Var v a))) >> (return $ Just (V v))
   readVar        v = gets variable >>= (\(Var _ a) -> return $ cast a)
   writeVar (V v) a = modify (\(TestState eis os _) -> (TestState eis os (Var v a))) >> return True
   delVar        _  = return True

eventsEval :: Evaluate TestIO TestState () -> TestIO ()
eventsEval eval = do
   s <- get
   let (EvalEnv eis s' _ _) = runIdentity $ flip execStateT (EvalEnv (eventInfos s) s (void . evalEvents) undefined) $ do
       res <- runErrorT eval
       case res of
         Right a -> return a
         Left e -> error $ show "error occured"
   put s'

evalEvents :: TestIO a -> Evaluate TestIO TestState a
evalEvents (TestIO tio) = do
   (EvalEnv eis s f g) <- get
   let (a, s') = unsafePerformIO $ runStateT tio s
   put (EvalEnv (eventInfos s') s' f g)
   return a

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> TestIO ()) -> TestIO EventNumber
evOnEvent ev h = do
   (TestState evs os vs) <- get
   let en = getFreeNumber (map _eventNumber evs)
   put (TestState ((EventInfo en ev h SActive []) : evs) os vs)
   return en

evDelEvent :: EventNumber -> TestIO Bool
evDelEvent en = do
   (TestState evs os vs) <- get
   case find ((== en) . getL eventNumber) evs of
      Nothing -> return False
      Just eh -> case _evStatus eh of
         SActive -> do
            put (TestState (replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs) os vs)
            return True
         SDeleted -> return False

execEvents :: (Signal e) => TestIO a -> e -> SignalDataType e -> [String]
execEvents r f d = outputs $ _execState $ runIdentity $ flip execStateT (EvalEnv [] (TestState [] [] (Var "" "")) (void . evalEvents) undefined) $ do
   res <- runErrorT $ do
      void $ evalEvents r
      triggerEvent f d
      return ()
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

exec :: TestIO a -> [String]
exec r = outputs $ _execState $ runIdentity $ flip execStateT (EvalEnv [] (TestState [] [] (Var "" "")) (void . evalEvents) undefined) $ do
   res <- runErrorT $ do
      void $ evalEvents r
      return ()
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

putStrLn' :: String -> TestIO ()
putStrLn' s = modify (\(TestState is ss vs) -> (TestState is (s:ss) vs))

allTests = [testSingleInputEx, testMultipleInputsEx, testInputStringEx, testSendMessageEx, testSendMessageEx2, testAPICallEx, testAPICallEx2, testUserInputWriteEx]

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded, Data)

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

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded, Data)

-- Test user input + variable read/write
testUserInputWrite :: TestIO ()
testUserInputWrite = do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (messageEvent (Msg "voted" :: Msg ())) h2
    void $ onEvent_ (SignalEvent $ Input "Vote for" (Radio [(Me, "Me"), (You, "You")])) h1 where
        h1 a = do
            writeVar (V "vote") (Just a)
            sendMessage (Msg "voted") ()
        h2 _ = do
            a <- readVar (V "vote")
            void $ case a of
                Just (Just Me) -> putStrLn' "voted Me"
                _ -> putStrLn' "problem"


testUserInputWriteEx :: Bool
testUserInputWriteEx = "voted Me" `elem` g where
   g = execEvents testUserInputWrite (Input "Vote for" (Radio [(Me, "Me"), (You, "You")])) Me
