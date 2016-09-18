{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Imprevu.Test.Test where

import Control.Monad.State
import Control.Applicative
import Control.Shortcut
import Data.Typeable
import Data.List
import Data.Maybe
import Data.Time
import Imprevu.Events
import Imprevu.Inputs
import Imprevu.Variables
import Imprevu.Messages
import Imprevu.Event
import Imprevu.Evaluation.InputEval
import Imprevu.Test.TestMgt
import Prelude

-- * Tests

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: TestM ()
testSingleInput = void $ onInputRadio_ "Vote for Holland or Sarkozy" [Holland, Sarkozy] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

testSingleInputEx :: Bool
testSingleInputEx = "voted for Holland" `elem` g where
   g = execEvent testSingleInput (InputS (Radio "Vote for Holland or Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")]) 1) Holland

testMultipleInputs :: TestM ()
testMultipleInputs = void $ onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

testMultipleInputsEx :: Bool
testMultipleInputsEx = "voted for [Holland,Sarkozy]" `elem` g where
   g = execEvent testMultipleInputs (InputS (Checkbox "Vote for Holland and Sarkozy"  [(Holland, "Holland"), (Sarkozy, "Sarkozy")]) 1) [Holland, Sarkozy]

testInputString :: TestM ()
testInputString = void $ onInputText_ "Enter a number:" h 1 where
   h a = putStrLn' ("You entered: " ++ a)

testInputStringEx :: Bool
testInputStringEx = "You entered: 1" `elem` g where
   g = execEvent testInputString (InputS (Text "Enter a number:") 1) "1"

-- Test message
testSendMessage :: TestM ()
testSendMessage = do
    let msg = Signal "msg" :: Msg String
    onEvent_ (messageEvent msg) f
    sendMessage msg "toto" where
        f (a :: String) = putStrLn' a

testSendMessageEx :: Bool
testSendMessageEx = "toto" `elem` (exec testSendMessage)

testSendMessage2 :: TestM ()
testSendMessage2 = do
    onEvent_ (messageEvent (Signal "msg" :: Msg ())) $ const $ putStrLn' "Received"
    sendMessage_ "msg"

testSendMessageEx2 :: Bool
testSendMessageEx2 = "Received" `elem` (exec testSendMessage2)

testAPICall :: TestM ()
testAPICall = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    callAPI call "toto" putStrLn'

testAPICallEx :: Bool
testAPICallEx = "toto" `elem` (exec testAPICall)

testAPICall2 :: TestM ()
testAPICall2 = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    a <- callAPIBlocking call "toto"
    putStrLn' a

testAPICallEx2 :: Bool
testAPICallEx2 = "toto" `elem` (exec testAPICall2)

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test user input + variable read/write
testUserInputWrite :: TestM ()
testUserInputWrite = do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (messageEvent (Signal "voted" :: Msg ())) h2
    void $ onEvent_ (signalEvent $ Radio "Vote for" [(Me, "Me"), (You, "You")] :: EventM TestM Choice2) h1 where
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

testSumCompose :: TestM ()
testSumCompose = void $ onEvent_ (True <$ inputButton 1 "click here:" <|> False <$ inputButton 2 "") f where
   f a = putStrLn' $ show a

testSumComposeEx :: Bool
testSumComposeEx = "True" `elem` g where
   g = execInput testSumCompose 1 [SumL, AppR] (ButtonField "click here:") ButtonData

testProdCompose :: TestM ()
testProdCompose = void $ onEvent_ ((,) <$> inputText 1 "" <*> inputText 1 "") f where
   f a = putStrLn' $ show a

testProdComposeEx1 :: Bool
testProdComposeEx1 = null g where
   g = execInput testProdCompose 1 [AppR] (TextField "") (TextData "toto")

testProdComposeEx2 :: Bool
testProdComposeEx2 = "(\"toto\",\"tata\")" `elem` g where
   g = execInputs testProdCompose 1 [([AppL, AppR], (TextField ""), TextData "toto"), ([AppR], (TextField ""), TextData "tata")]

testTwoEvents :: TestM ()
testTwoEvents = do
   void $ onEvent_ (inputText 1 "") f
   void $ onEvent_ (inputText 1 "") f where
   f a = putStrLn' $ show a

testTwoEventsEx :: Bool
testTwoEventsEx = (length g) == 1 where
   g = execInput testTwoEvents 1 [] (TextField "") (TextData "toto")

testMonadicEvent :: TestM ()
testMonadicEvent = do
   let displayMsg a = putStrLn' a
   let e = do
       a <- inputText 1 ""
       guard (a == "coco1") >> inputText 1 ""
   void $ onEvent_ e displayMsg

testMonadicEventEx :: Bool
testMonadicEventEx = "coco2" `elem` g where
   g = execInputs testMonadicEvent 1 [([BindL], (TextField ""), TextData "coco1"), ([BindR, BindR], (TextField ""), TextData "coco2")]

testShorcutEvent :: TestM ()
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
playerEvent :: Player -> EventM TestM PlayerInfo
playerEvent p = SignalEvent $ Signal p

data Player    = Arrive | Leave deriving (Typeable, Show, Eq)

-- | informations on players
data PlayerInfo = PlayerInfo { _playerNumber :: Int}
                               deriving (Eq, Typeable, Show)

--This event waits for two identical signals to fire
testDoubleEvent :: TestM ()
testDoubleEvent = do
   let displayMsg a = putStrLn' $ show $ _playerNumber a
   let e :: EventM TestM PlayerInfo
       e = do
       playerEvent Arrive
       playerEvent Arrive
   void $ onEvent_ e displayMsg

testDoubleEvent2PlayerArrive :: [String]
testDoubleEvent2PlayerArrive = execEvents testDoubleEvent [(Signal Arrive, PlayerInfo 1), (Signal Arrive, PlayerInfo 2)]

testDoubleEventEx :: Bool
testDoubleEventEx = "2" `elem` testDoubleEvent2PlayerArrive

-- * Time events

testTimeEvent :: TestM ()
testTimeEvent = void $ onEvent_ (timeEvent date1) f where
   f _ = putStrLn' $ show date1

testTimeEventEx :: Bool
testTimeEventEx = (show date1) `elem` g where
   g = execEvent testTimeEvent (Signal date1) date1

testTimeEvent2 :: TestM ()
testTimeEvent2 = schedule' [date1, date2] (putStrLn' . show)

testTimeEventEx2 :: Bool
testTimeEventEx2 = ((show date1) `elem` g) && ((show date2) `elem` g) where
  g = execEvents testTimeEvent2 [(Signal date1, date1),(Signal date2, date2)]

testTime :: TestM ()
testTime = do
  t <- liftIO Data.Time.getCurrentTime
  void $ onEvent_ (True <$ inputButton 1 "click here before 5 seconds:" <|> False <$ (timeEvent $ addUTCTime 5 t)) f where
   f a = putStrLn' $ show a
