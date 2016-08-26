{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test where

import Imprevu.Events
import Imprevu.Inputs
import Imprevu.SysMgt
import Imprevu.Variables
import Imprevu.Messages
import Imprevu.Internal.Event
import Imprevu.Internal.EventEval
import Imprevu.Internal.InputEval
import Imprevu.Internal.Utils
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Control.Shortcut
import Data.Typeable
import Data.List
import Data.Maybe
import Control.Lens
import Data.Time
import Control.Concurrent.STM
import System.IO.Unsafe
import System.Random
import Distribution.TestSuite

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

execEvent :: (Show s, Typeable s, Show e, Typeable e, Eq s, Eq e) => TestIO a -> Signal s e -> e -> [String]
execEvent r f d = execEvents r [(f,d)]

execEvents :: (Show s, Typeable s, Show e, Typeable e, Eq s, Eq e) => TestIO a -> [(Signal s e, e)] -> [String]
execEvents r sds = outputs $ _evalEnv $ runIdentity $ flip execStateT defaultEvalEnv $ do
   res <- runExceptT $ do
      void $ evalEvents r
      mapM (\(f,d) -> triggerEvent f d) sds
      return ()
   case res of
      Right a -> return a
      Left e -> error $ show "error occured"

execInput :: TestIO a -> EventNumber -> SignalAddress -> InputView -> InputDataView -> [String]
execInput r en sa ff ide = outputs $ _evalEnv $ runIdentity $ flip execStateT defaultEvalEnv $ do
   res <- runExceptT $ do
      void $ evalEvents r
      triggerInput ff ide sa en
      return ()
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

testSingleInputEx :: TestInstance
testSingleInputEx = test ("voted for Holland" `elem` g) "single input" where
   g = execEvent testSingleInput (InputS (Radio "Vote for Holland or Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) Holland

testMultipleInputs :: TestIO ()
testMultipleInputs = void $ onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

testMultipleInputsEx :: TestInstance
testMultipleInputsEx = test ("voted for [Holland,Sarkozy]" `elem` g) "multiple inputs" where
   g = execEvent testMultipleInputs (InputS (Checkbox "Vote for Holland and Sarkozy"  [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) [Holland, Sarkozy]

testInputString :: TestIO ()
testInputString = void $ onInputText_ "Enter a number:" h 1 where
   h a = putStrLn' ("You entered: " ++ a)

testInputStringEx :: TestInstance
testInputStringEx = test ("You entered: 1" `elem` g) "input string" where
   g = execEvent testInputString (InputS $ Text "Enter a number:") "1"

-- Test message
testSendMessage :: TestIO ()
testSendMessage = do
    let msg = Signal "msg" :: Msg String
    onEvent_ (messageEvent msg) f
    sendMessage msg "toto" where
        f (a :: String) = putStrLn' a

testSendMessageEx :: TestInstance
testSendMessageEx = test ("toto" `elem` (exec testSendMessage)) "send message"

testSendMessage2 :: TestIO ()
testSendMessage2 = do
    onEvent_ (messageEvent (Signal "msg" :: Msg ())) $ const $ putStrLn' "Received"
    sendMessage_ "msg"

testSendMessageEx2 :: TestInstance
testSendMessageEx2 = test ("Received" `elem` (exec testSendMessage2)) "send message 2"

testAPICall :: TestIO ()
testAPICall = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    callAPI call "toto" putStrLn'

testAPICallEx :: TestInstance
testAPICallEx = test ("toto" `elem` (exec testAPICall)) "API Call"

testAPICall2 :: TestIO ()
testAPICall2 = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    a <- callAPIBlocking call "toto"
    putStrLn' a

testAPICallEx2 :: TestInstance
testAPICallEx2 = test ("toto" `elem` (exec testAPICall2)) "API Call 2"

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


testUserInputWriteEx :: TestInstance
testUserInputWriteEx = test ("voted Me" `elem` g) "User input write" where
   g = execEvent testUserInputWrite (Signal $ Radio "Vote for" [(Me, "Me"), (You, "You")]) Me

-- Event composition

testSumCompose :: TestIO ()
testSumCompose = void $ onEvent_ (True <$ inputButton 1 "click here:" <|> False <$ inputButton 2 "") f where
   f a = putStrLn' $ show a

testSumComposeEx :: TestInstance
testSumComposeEx = test ("True" `elem` g) "sum compose" where
   g = execInput testSumCompose 1 [SumL, AppR] (ButtonField "click here:") ButtonData

testProdCompose :: TestIO ()
testProdCompose = void $ onEvent_ ((,) <$> inputText 1 "" <*> inputText 1 "") f where
   f a = putStrLn' $ show a

testProdComposeEx1 :: TestInstance
testProdComposeEx1 = test (null g) "prod compose" where
   g = execInput testProdCompose 1 [AppR] (TextField "") (TextData "toto")

testProdComposeEx2 :: TestInstance
testProdComposeEx2 = test ("(\"toto\",\"tata\")" `elem` g) "prod compose 2" where
   g = execInputs testProdCompose 1 [([AppL, AppR], (TextField ""), TextData "toto"), ([AppR], (TextField ""), TextData "tata")]

testTwoEvents :: TestIO ()
testTwoEvents = do
   void $ onEvent_ (inputText 1 "") f
   void $ onEvent_ (inputText 1 "") f where
   f a = putStrLn' $ show a

testTwoEventsEx :: TestInstance
testTwoEventsEx = test ((length g) == 1) "two events" where
   g = execInput testTwoEvents 1 [] (TextField "") (TextData "toto")

testMonadicEvent :: TestIO ()
testMonadicEvent = do
   let displayMsg a = putStrLn' a
   let e = do
       a <- inputText 1 ""
       guard (a == "coco1") >> inputText 1 ""
   void $ onEvent_ e displayMsg

testMonadicEventEx :: TestInstance
testMonadicEventEx = test ("coco2" `elem` g) "monadic event" where
   g = execInputs testMonadicEvent 1 [([BindL], (TextField ""), TextData "coco1"), ([BindR, BindR], (TextField ""), TextData "coco2")]

testShorcutEvent :: TestIO ()
testShorcutEvent = do
   let displayMsg a = putStrLn' (concat $ catMaybes a)
   let e = do
       let a = inputText 1 "a"
       let b = inputText 1 "b"
       shortcut [a,b] (\as -> length (filter isJust as) >= 1)
   void $ onEvent_ e displayMsg

testShorcutEventEx :: TestInstance
testShorcutEventEx = test ("coco1" `elem` g) "shortcut event" where
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

testDoubleEventEx :: TestInstance
testDoubleEventEx = test ("2" `elem` testDoubleEvent2PlayerArrive) "double event"

test :: Bool -> String -> TestInstance
test p t = TestInstance
   { run = return $ Finished $ if p then Pass else Fail ""
   , name = t
   , tags = []
   , options = []
   , setOption = \_ _ -> Right $ test p t
   }

tests :: IO [Test]
tests = return [Test testSingleInputEx,
           Test testMultipleInputsEx,
           Test testInputStringEx,
           Test testSendMessageEx,
           Test testSendMessageEx2,
           Test testAPICallEx,
           Test testAPICallEx2,
           Test testUserInputWriteEx,
           Test testSumComposeEx,
           Test testSumComposeEx,
           Test testProdComposeEx1,
           Test testProdComposeEx2,
           Test testTwoEventsEx,
           Test testMonadicEventEx,
           Test testShorcutEventEx,
           Test testDoubleEventEx]

