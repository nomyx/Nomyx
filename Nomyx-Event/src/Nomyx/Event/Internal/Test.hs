{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Event.Nomyx.Engine.Test where

import Event.Nomyx.Expression
import Event.Nomyx.Engine.Evaluation
import Event.Nomyx.Engine.EventEval
import Event.Nomyx.Engine.Types
import Event.Nomyx.Engine.Utils
import Control.Monad.State
import Data.Typeable
import Data.Function hiding ((.))
import Data.Maybe
import Control.Applicative
import Control.Lens
import Control.Shortcut
import System.Random
import Data.Time hiding (getCurrentTime)

date1, date2, date3 :: UTCTime
date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"
date3 = parse822Time "Tue, 02 Sep 1997 11:00:00 -0400"


execRuleEvent :: (Show e, Typeable e) => Nomex a -> Signal e -> e -> Game
execRuleEvent r f d = execState (runSystemEval' $ evalNomex r >> triggerEvent f d) testGame

execRuleEvents :: (Show e, Typeable e) => Nomex a -> [(Signal e, e)] -> Game
execRuleEvents f eds = execState (runSystemEval' $ evalNomex f >> mapM (\(a,b) -> triggerEvent a b) eds) testGame

execRuleInput :: Nomex a -> EventNumber -> SignalAddress -> FormField -> InputData -> Game
execRuleInput r en sa ff ide = execState (runSystemEval' $ evalNomex r >> triggerInput ff ide sa en) testGame

execRuleInputs :: Nomex a -> EventNumber -> [(SignalAddress, FormField, InputData)] -> Game
execRuleInputs r en fads = execState (runSystemEval' $ evalNomex r >> mapM (\(sa, ff, ide) -> triggerInput ff ide sa en) fads) testGame

execRuleGame :: Nomex a -> Game -> Game
execRuleGame r g = execState (runSystemEval' $ void $ evalNomex r) g

execRuleEventGame :: (Show e, Typeable e) => Nomex a -> Signal e -> e -> Game -> Game
execRuleEventGame r f d g = execState (runSystemEval' $ evalNomex r >> (triggerEvent f d)) g

execRule :: Nomex a -> Game
execRule r = execRuleGame r testGame


tests :: [([Char], Bool)]
tests = [
         ("test single input", testSingleInputEx),
         ("test multiple input", testMultipleInputsEx),
         ("test input string", testInputStringEx),
         ("test send messsage", testSendMessageEx),
         ("test send message 2", testSendMessageEx2),
         ("test API call", testAPICallEx),
         ("test API call 2", testAPICallEx2),
         ("test time event", testTimeEventEx),
         ("test time event 2", testTimeEventEx2),
         ("test composed event sum", testSumComposeEx),
         ("test composed event prod 1", testProdComposeEx1),
         ("test composed event prod 2", testProdComposeEx2),
         ("test two separate events", testTwoEventsEx),
         ("test monadic event", testMonadicEventEx),
         ("test monadic event2", testMonadicEventEx2),
         ("test shortcut event", testShorcutEventEx),
         ("test double event", testDoubleEventEx)
         ]

allTests :: Bool
allTests = all snd tests

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: Rule
testSingleInput = void $ onInputRadio_ "Vote for Holland or Sarkozy" [Holland, Sarkozy] h 1 where
   h a = void $ newOutput (Just 1) (return $ "voted for " ++ show a)

testSingleInputEx :: Bool
testSingleInputEx = isOutput "voted for Holland" g where
   g = execRuleEvent testSingleInput (Input 1 "Vote for Holland or Sarkozy" (Radio [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) Holland

testMultipleInputs :: Rule
testMultipleInputs = void $ onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
   h a = void $ newOutput (Just 1) (return $ "voted for " ++ show a)

testMultipleInputsEx :: Bool
testMultipleInputsEx = isOutput "voted for [Holland,Sarkozy]" g where
   g = execRuleEvent testMultipleInputs (Input 1 "Vote for Holland and Sarkozy" (Checkbox [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) [Holland, Sarkozy]

testInputString :: Rule
testInputString = void $ onInputText_ "Enter a number:" h 1 where
   h a = void $ newOutput (Just 1) (return $ "You entered: " ++ a)

testInputStringEx :: Bool
testInputStringEx = isOutput "You entered: 1" g where
   g = execRuleEvent testInputString (Input 1 "Enter a number:" Text) "1"

-- Test message
testSendMessage :: Rule
testSendMessage = do
    let msg = Msg "msg" :: Msg String
    onEvent_ (messageEvent msg) f
    sendMessage msg "toto" where
        f (a :: String) = void $ newOutput (Just 1) (return a)

testSendMessageEx :: Bool
testSendMessageEx = isOutput "toto" (execRule testSendMessage)

testSendMessage2 :: Rule
testSendMessage2 = do
    onEvent_ (messageEvent (Msg "msg" :: Msg ())) $ const $ void $ newOutput (Just 1) (return "Received")
    sendMessage_ "msg"

testSendMessageEx2 :: Bool
testSendMessageEx2 = isOutput "Received" (execRule testSendMessage2)

testAPICall :: Rule
testAPICall = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    callAPI call "toto" outputAll_

testAPICallEx :: Bool
testAPICallEx = isOutput "toto" (execRule testAPICall)

testAPICall2 :: Rule
testAPICall2 = do
    let call = APICall "test" :: APICall String String
    onAPICall call return
    a <- callAPIBlocking call "toto"
    outputAll_ a

testAPICallEx2 :: Bool
testAPICallEx2 = isOutput "toto" (execRule testAPICall2)


--Time tests

testTimeEvent :: Rule
testTimeEvent = void $ onEvent_ (timeEvent date1) f where
   f _ = outputAll_ $ show date1

testTimeEventEx :: Bool
testTimeEventEx = isOutput (show date1) g where
   g = execRuleEvent testTimeEvent (Time date1) date1

testTimeEvent2 :: Nomex ()
testTimeEvent2 = schedule' [date1, date2] (outputAll_ . show)

testTimeEventEx2 :: Bool
testTimeEventEx2 = isOutput (show date1) g && isOutput (show date2) g where
    g = execState (runSystemEval' $ evalNomex testTimeEvent2 >> void gameEvs) testGame
    gameEvs = do
        evTriggerTime date1
        evTriggerTime date2


-- Event composition

testSumCompose :: Rule
testSumCompose = void $ onEvent_ (True <$ inputButton 1 "click here:" <|> False <$ inputButton 2 "") f where
   f a = outputAll_ $ show a

testSumComposeEx :: Bool
testSumComposeEx = isOutput "True" g where
   g = execRuleInput testSumCompose 1 [SumL, AppR] (ButtonField 1 "click here:") ButtonData


testProdCompose :: Rule
testProdCompose = void $ onEvent_ ((,) <$> inputText 1 "" <*> inputText 1 "") f where
   f a = outputAll_ $ show a

testProdComposeEx1 :: Bool
testProdComposeEx1 = null $ allOutputs g where
   g = execRuleInput testProdCompose 1 [AppR] (TextField 1 "") (TextData "toto")

testProdComposeEx2 :: Bool
testProdComposeEx2 = isOutput "(\"toto\",\"tata\")" g where
   g = execRuleInputs testProdCompose 1 [([AppL, AppR], (TextField 1 ""), TextData "toto"), ([AppR], (TextField 1 ""), TextData "tata")]

testTwoEvents :: Rule
testTwoEvents = do
   void $ onEvent_ (inputText 1 "") f
   void $ onEvent_ (inputText 1 "") f where
   f a = outputAll_ $ show a

testTwoEventsEx :: Bool
testTwoEventsEx = (length $ allOutputs g) == 1 where
   g = execRuleInput testTwoEvents 1 [] (TextField 1 "") (TextData "toto")

testMonadicEvent :: Rule
testMonadicEvent = do
   let displayMsg a = void $ newOutput_ Nothing a
   let e = do
       a <- inputText 1 ""
       guard (a == "coco1") >> inputText 1 ""
   void $ onEvent_ e displayMsg

testMonadicEventEx :: Bool
testMonadicEventEx = isOutput "coco2" g where
   g = execRuleInputs testMonadicEvent 1 [([BindL], (TextField 1 ""), TextData "coco1"), ([BindR, BindR], (TextField 1 ""), TextData "coco2")]

testMonadicEvent2 :: Rule
testMonadicEvent2 = do
   let displayMsg a = void $ newOutput_ Nothing a
   let e = do
       playerEvent Arrive
       inputText 1 ""
   void $ onEvent_ e displayMsg

testMonadicEvent2PlayerArrive :: Game
testMonadicEvent2PlayerArrive = flip execState testGame {_players = []} $ runSystemEval' $ do
    addActivateRule testMonadicEvent2 1
    addPlayer (PlayerInfo 1 "coco 1" Nothing)
    triggerInput (TextField 1 "") (TextData "coco2") [BindR] 1

testMonadicEventEx2 :: Bool
testMonadicEventEx2 = isOutput "coco2" testMonadicEvent2PlayerArrive

testShorcutEvent :: Rule
testShorcutEvent = do
   let displayMsg a = void $ newOutput_ Nothing (concat $ catMaybes a)
   let e = do
       let a = inputText 1 "a"
       let b = inputText 1 "b"
       shortcut [a,b] (\as -> length (filter isJust as) >= 1)
   void $ onEvent_ e displayMsg

testShorcutEventEx :: Bool
testShorcutEventEx = isOutput "coco1" g where
   g = execRuleInputs testShorcutEvent 1 [([Shortcut], (TextField 1 "a"), TextData "coco1")]

--This event waits for two identical signals to fire
testDoubleEvent :: Rule
testDoubleEvent = do
   let displayMsg a = void $ newOutput_ Nothing (_playerName a)
   let e :: Event PlayerInfo
       e = do
       playerEvent Arrive
       playerEvent Arrive
   void $ onEvent_ e displayMsg

testDoubleEvent2PlayerArrive :: Game
testDoubleEvent2PlayerArrive = flip execState testGame {_players = []} $ runSystemEval' $ do
    addActivateRule testDoubleEvent 1
    addPlayer (PlayerInfo 1 "coco1" Nothing)
    addPlayer (PlayerInfo 2 "coco2" Nothing)

testDoubleEventEx :: Bool
testDoubleEventEx = isOutput "coco2" testDoubleEvent2PlayerArrive

--Get all event numbers of type choice (radio button)
getChoiceEvents :: State EvalEnv [(EventNumber, SignalAddress, PlayerNumber, String)]
getChoiceEvents = do
   evs <- use (eGame . events)
   g <- use eGame
   return $ [(_eventNumber ev, fa, pn, t) | ev <- evs, (fa, pn, t) <- getInputChoices ev g]

getInputChoices :: EventInfo -> Game -> [(SignalAddress, PlayerNumber, String)]
getInputChoices ei g = mapMaybe isInput (getRemainingSignals ei g) where
   isInput :: (SignalAddress, SomeSignal) -> Maybe (SignalAddress, PlayerNumber, String)
   isInput (fa, (SomeSignal (Input pn t (Radio _)))) = Just (fa, pn, t)
   isInput _ = Nothing

--Get all event numbers of type text (text field)
getTextEvents :: State Game [(EventNumber, SignalAddress)]
getTextEvents = do
   evs <- use events
   g <- get
   return $ [(_eventNumber ev, fa) | ev <- evs, fa <- getInputTexts ev g]

getInputTexts :: EventInfo -> Game -> [SignalAddress]
getInputTexts ei g = mapMaybe isInput (getRemainingSignals ei g) where
   isInput :: (t, SomeSignal) -> Maybe t
   isInput (fa, (SomeSignal (Input _ _ Text))) = Just fa
   isInput _ = Nothing

