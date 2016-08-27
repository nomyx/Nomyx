{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Imprevu.Test where

import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Control.Shortcut
import Control.Lens
import Data.Typeable
import Data.List
import Data.Maybe
import Data.Time
import Imprevu.Events
import Imprevu.Inputs
import Imprevu.SysMgt
import Imprevu.Variables
import Imprevu.Messages
import Imprevu.Evaluation.Event
import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.InputEval
import Imprevu.Evaluation.Utils
import System.IO.Unsafe
import System.Random

--type Evaluate n s a = ErrorT String (State (EvalEnv n s)) a
data TestState = TestState {eventInfos :: [EventInfo TestIO],
                            outputs    :: [String],
                            variable  :: Var}
                            deriving Show

newtype TestIO a = TestIO {unTestIO :: StateT TestState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TestState)

instance HasEvents TestIO TestState where
  getEvents = eventInfos
  setEvents eis (TestState _ os vs) = (TestState eis os vs)

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a) =>
   Var { vName :: String,
         vData :: a}

deriving instance (Show Var)

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
   let (EvalEnv s' _ _) = runIdentity $ flip execStateT (EvalEnv s (void . evalEvents) undefined) $ do
       res <- runExceptT eval
       case res of
         Right a -> return a
         Left e -> error $ show "error occured"
   put s'

evalEvents :: TestIO a -> Evaluate TestIO TestState a
evalEvents (TestIO tio) = do
   (EvalEnv s f g) <- get
   let (a, s') = unsafePerformIO $ runStateT tio s
   put (EvalEnv s' f g)
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


defaultEvalEnv = EvalEnv (TestState [] [] (Var "" "")) (void . evalEvents) undefined

execEvents' :: TestIO a -> TestState -> TestState
execEvents' r ts = _evalEnv $ runIdentity $ flip execStateT (EvalEnv ts (void . evalEvents) undefined) $ do
   res <- runExceptT $ do
      void $ evalEvents r
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

execEvent :: (Show s, Typeable s, Show e, Typeable e, Eq s, Eq e) => TestIO a -> Signal s e -> e -> [String]
execEvent r f d = execEvents r [(f,d)]

execEvents :: (Show s, Typeable s, Show e, Typeable e, Eq s, Eq e) => TestIO a -> [(Signal s e, e)] -> [String]
execEvents r sds = outputs $ _evalEnv $ runIdentity $ flip execStateT defaultEvalEnv $ do
   res <- runExceptT $ do
      void $ evalEvents r
      mapM_ (\(f,d) -> triggerEvent f d) sds
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

execInput :: TestIO a -> EventNumber -> SignalAddress -> InputView -> InputDataView -> [String]
execInput r en sa ff ide = outputs $ _evalEnv $ runIdentity $ flip execStateT defaultEvalEnv $ do
   res <- runExceptT $ do
      void $ evalEvents r
      triggerInput ff ide sa en
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

execInputs :: TestIO a -> EventNumber -> [(SignalAddress, InputView, InputDataView)] -> [String]
execInputs r en fads = outputs $ _evalEnv $ runIdentity $ flip execStateT defaultEvalEnv $ do
   res <- runExceptT $ do
      void $ evalEvents r
      mapM (\(sa, ff, ide) -> triggerInput ff ide sa en) fads
      return ()
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

exec :: TestIO a -> [String]
exec r = outputs $ _evalEnv $ runIdentity $ flip execStateT defaultEvalEnv $ do
   res <- runExceptT $ do
      void $ evalEvents r
      return ()
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

putStrLn' :: String -> TestIO ()
putStrLn' s = modify (\(TestState is ss vs) -> (TestState is (s:ss) vs))


data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: TestIO ()
testSingleInput = void $ onInputRadio_ "Vote for Holland or Sarkozy" [Holland, Sarkozy] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

testSingleInputEx :: Bool
testSingleInputEx = "voted for Holland" `elem` g where
   g = execEvent testSingleInput (InputS (Radio "Vote for Holland or Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) Holland

testMultipleInputs :: TestIO ()
testMultipleInputs = void $ onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

testMultipleInputsEx :: Bool
testMultipleInputsEx = "voted for [Holland,Sarkozy]" `elem` g where
   g = execEvent testMultipleInputs (InputS (Checkbox "Vote for Holland and Sarkozy"  [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) [Holland, Sarkozy]

testInputString :: TestIO ()
testInputString = void $ onInputText_ "Enter a number:" h 1 where
   h a = putStrLn' ("You entered: " ++ a)

testInputStringEx :: Bool
testInputStringEx = "You entered: 1" `elem` g where
   g = execEvent testInputString (InputS $ Text "Enter a number:") "1"

-- Test message
testSendMessage :: TestIO ()
testSendMessage = do
    let msg = Signal "msg" :: Msg String
    onEvent_ (messageEvent msg) f
    sendMessage msg "toto" where
        f (a :: String) = putStrLn' a

testSendMessageEx :: Bool
testSendMessageEx = "toto" `elem` (exec testSendMessage)

testSendMessage2 :: TestIO ()
testSendMessage2 = do
    onEvent_ (messageEvent (Signal "msg" :: Msg ())) $ const $ putStrLn' "Received"
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

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test user input + variable read/write
testUserInputWrite :: TestIO ()
testUserInputWrite = do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (messageEvent (Signal "voted" :: Msg ())) h2
    void $ onEvent_ (signalEvent $ Radio "Vote for" [(Me, "Me"), (You, "You")] :: Event Choice2) h1 where
        h1 a = do
            writeVar (V "vote") (Just a)
            sendMessage (Signal "voted") ()
        h2 _ = do
            a <- readVar (V "vote")
            void $ case a of
                Just (Just Me) -> putStrLn' "voted Me"
                _ -> putStrLn' "problem"


testUserInputWriteEx :: Bool
testUserInputWriteEx = "voted Me" `elem` g where
   g = execEvent testUserInputWrite (Signal $ Radio "Vote for" [(Me, "Me"), (You, "You")]) Me

-- Event composition

testSumCompose :: TestIO ()
testSumCompose = void $ onEvent_ (True <$ inputButton 1 "click here:" <|> False <$ inputButton 2 "") f where
   f a = putStrLn' $ show a

testSumComposeEx :: Bool
testSumComposeEx = "True" `elem` g where
   g = execInput testSumCompose 1 [SumL, AppR] (ButtonField "click here:") ButtonData

testProdCompose :: TestIO ()
testProdCompose = void $ onEvent_ ((,) <$> inputText 1 "" <*> inputText 1 "") f where
   f a = putStrLn' $ show a

testProdComposeEx1 :: Bool
testProdComposeEx1 = null g where
   g = execInput testProdCompose 1 [AppR] (TextField "") (TextData "toto")

testProdComposeEx2 :: Bool
testProdComposeEx2 = "(\"toto\",\"tata\")" `elem` g where
   g = execInputs testProdCompose 1 [([AppL, AppR], (TextField ""), TextData "toto"), ([AppR], (TextField ""), TextData "tata")]

testTwoEvents :: TestIO ()
testTwoEvents = do
   void $ onEvent_ (inputText 1 "") f
   void $ onEvent_ (inputText 1 "") f where
   f a = putStrLn' $ show a

testTwoEventsEx :: Bool
testTwoEventsEx = (length g) == 1 where
   g = execInput testTwoEvents 1 [] (TextField "") (TextData "toto")

testMonadicEvent :: TestIO ()
testMonadicEvent = do
   let displayMsg a = putStrLn' a
   let e = do
       a <- inputText 1 ""
       guard (a == "coco1") >> inputText 1 ""
   void $ onEvent_ e displayMsg

testMonadicEventEx :: Bool
testMonadicEventEx = "coco2" `elem` g where
   g = execInputs testMonadicEvent 1 [([BindL], (TextField ""), TextData "coco1"), ([BindR, BindR], (TextField ""), TextData "coco2")]

testShorcutEvent :: TestIO ()
testShorcutEvent = do
   let displayMsg a = putStrLn' (concat $ catMaybes a)
   let e = do
       let a = inputText 1 "a"
       let b = inputText 1 "b"
       shortcut [a,b] (\as -> length (filter isJust as) >= 1)
   void $ onEvent_ e displayMsg

testShorcutEventEx :: Bool
testShorcutEventEx = "coco1" `elem` g where
   g = execInputs testShorcutEvent 1 [([Shortcut], (TextField "a"), TextData "coco1")]

-- | Build a event firing when a player arrives or leaves
playerEvent :: Player -> Event PlayerInfo
playerEvent p = SignalEvent $ Signal p

data Player    = Arrive | Leave deriving (Typeable, Show, Eq)

-- | informations on players
data PlayerInfo = PlayerInfo { _playerNumber :: Int}
                               deriving (Eq, Typeable, Show)

--This event waits for two identical signals to fire
testDoubleEvent :: TestIO ()
testDoubleEvent = do
   let displayMsg a = putStrLn' $ show $ _playerNumber a
   let e :: Event PlayerInfo
       e = do
       playerEvent Arrive
       playerEvent Arrive
   void $ onEvent_ e displayMsg

testDoubleEvent2PlayerArrive :: [String]
testDoubleEvent2PlayerArrive = execEvents testDoubleEvent [(Signal Arrive, PlayerInfo 1), (Signal Arrive, PlayerInfo 2)]

testDoubleEventEx :: Bool
testDoubleEventEx = "2" `elem` testDoubleEvent2PlayerArrive


