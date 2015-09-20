{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Nomyx.Core.Engine.Test where

import Language.Nomyx.Expression
import Language.Nomyx.Variables
import Language.Nomyx.Rules
import Language.Nomyx.Events
import Language.Nomyx.Outputs
import Language.Nomyx.Inputs
import Language.Nomyx.Vote
import Language.Nomyx.Messages
import Language.Nomyx.Examples
import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.EventEval
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Utils
import Control.Monad.State
import Data.Typeable
import Data.Function hiding ((.))
import Data.Maybe
import Control.Applicative
import Control.Lens
import Control.Shortcut
import System.Random

date1, date2, date3 :: UTCTime
date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"
date3 = parse822Time "Tue, 02 Sep 1997 11:00:00 -0400"

testGame :: Game
testGame = Game { _gameName      = "test",
                  _gameDesc      = GameDesc "test" "test",
                  _rules         = [],
                  _players       = [PlayerInfo 1 "coco" Nothing],
                  _variables     = [],
                  _events        = [],
                  _outputs       = [],
                  _victory       = Nothing,
                  _logs          = [],
                  _currentTime   = date1,
                  _randomGen     = mkStdGen 0}

testRule :: RuleInfo
testRule = RuleInfo  { _rNumber       = 0,
                       _rProposedBy   = 0,
                       _rRule         = return (),
                       _rStatus       = Pending,
                       _rAssessedBy   = Nothing,
                       _rRuleDetails      = RuleDetails {_rName = "test",
                                                 _rDescription = "test",
                                                 _rRuleCode = "",
                                                 _rAuthor = "",
                                                 _rPicture = Nothing,
                                                 _rCategory = []}}

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

addActivateRule :: Rule -> RuleNumber -> Evaluate ()
addActivateRule rf rn = do
   let rule = testRule & (rRuleDetails . rName) .~ "testRule"
                       & rRule   .~ rf
                       & rNumber .~ rn
                       & rStatus .~ Pending
   evAddRule rule
   evActivateRule (_rNumber rule)
   return ()

tests :: [([Char], Bool)]
tests = [("test var 1", testVarEx1),
         ("test var 2", testVarEx2),
         ("test var 3", testVarEx3),
         ("test var 4", testVarEx4),
         ("test var 5", testVarEx5),
         ("test single input", testSingleInputEx),
         ("test multiple input", testMultipleInputsEx),
         ("test input string", testInputStringEx),
         ("test send messsage", testSendMessageEx),
         ("test send message 2", testSendMessageEx2),
         ("test API call", testAPICallEx),
         ("test API call 2", testAPICallEx2),
         ("test user input write", testUserInputWriteEx),
         ("test activate rule", testActivateRuleEx),
         ("test auto activate", testAutoActivateEx),
         ("test time event", testTimeEventEx),
         ("test time event 2", testTimeEventEx2),
         ("test delete rule", testDeleteRuleEx1),
         ("test victory rule", testVictoryEx1),
         ("test assess on vote complete 1", testVoteAssessOnVoteComplete1),
         ("test assess on vote complete 2", testVoteAssessOnVoteComplete2),
         ("test assess on every vote 1", testVoteAssessOnEveryVote1),
         ("test assess on every vote 2", testVoteAssessOnEveryVote2),
         ("test assess on every vote 3", testVoteAssessOnEveryVote3),
         ("test assess on every vote 4", testVoteAssessOnEveryVote4),
         ("test majority with", testVoteMajorityWith),
         ("test number positive votes", testVoteNumberPositiveVotes),
         ("test vote with quorum 1", testVoteWithQuorum1),
         ("test assess on time limit 1", testVoteAssessOnTimeLimit1),
         ("test assess on time limit 2", testVoteAssessOnTimeLimit2),
         ("test assess on time limit 3", testVoteAssessOnTimeLimit3),
         ("test assess on time limit 4", testVoteAssessOnTimeLimit4),
         ("test assess on time limit 5", testVoteAssessOnTimeLimit5),
         ("test vote player arrives", testVotePlayerArriveEx),
         ("test vote player leaves", testVotePlayerLeaveEx),
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

--Test variable creation
testVar1 :: Rule
testVar1 = do
   NewVar "toto" (1::Integer)
   return ()

testVarEx1 :: Bool
testVarEx1 = True --(variables ^$ execRuleFunc testVar1) == [(Var 0 "toto" (1::Integer))]

--Test variable deleting
testVar2 :: Rule
testVar2 = do
   var <- newVar_ "toto" (1::Int)
   delVar var
   return ()

testVarEx2 :: Bool
testVarEx2 = True --_variables (execRuleFunc testVar2) == []

--Test variable reading
testVar3 :: Rule
testVar3 = do
   var <- newVar_ "toto" (1::Int)
   a <- liftEffect $ readVar var
   case a of
      Just (1::Int) -> void $ newOutput (Just 1) (return "ok")
      _ -> void $ newOutput (Just 1) (return "nok")

testVarEx3 :: Bool
testVarEx3 = isOutput "ok" (execRule testVar3)

--Test variable writing
testVar4 :: Rule
testVar4 = do
   var <- newVar_ "toto" (1::Int)
   writeVar var (2::Int)
   a <- liftEffect $ readVar var
   case a of
      Just (2::Int) -> void $ newOutput (Just 1) (return "ok")
      _ -> void $ newOutput (Just 1) (return "nok")

testVarEx4 :: Bool
testVarEx4 = isOutput "ok" (execRule testVar4)

--Test variable writing
testVar5 :: Rule
testVar5 = do
   var <- newVar_ "toto" ([]::[Int])
   writeVar var ([1]::[Int])
   a <- liftEffect $ readVar var
   case a of
      Just (a::[Int]) -> void $ writeVar var (2:a)
      Nothing         -> void $ newOutput (Just 1) (return "nok")

testVarEx5 :: Bool
testVarEx5 = (show $ head $ _variables $ execRule testVar5) == "Rule number = 0, Name = \"toto\", Value = [2,1]\n"

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

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test user input + variable read/write
testUserInputWrite :: Rule
testUserInputWrite = do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (messageEvent (Msg "voted" :: Msg ())) h2
    void $ onEvent_ (SignalEvent $ Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")])) h1 where
        h1 a = do
            writeVar (V "vote") (Just a)
            SendMessage (Msg "voted") ()
        h2 _ = do
            a <- liftEffect $ readVar (V "vote")
            void $ case a of
                Just (Just Me) -> newOutput (Just 1) (return "voted Me")
                _ -> newOutput (Just 1) (return "problem")


testUserInputWriteEx :: Bool
testUserInputWriteEx = isOutput "voted Me" g where
   g = execRuleEvent testUserInputWrite (Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")])) Me

-- Test rule activation
testActivateRule :: Rule
testActivateRule = do
    a <- liftEffect GetRules
    when (_rStatus (head a) == Pending) $ void $ ActivateRule $ _rNumber (head a)


testActivateRuleEx :: Bool
testActivateRuleEx = _rStatus (head $ _rules (execRuleGame testActivateRule testGame {_rules=[testRule]}))  == Active

testAutoActivateEx :: Bool
testAutoActivateEx = _rStatus (head $ _rules (execRuleEventGame autoActivate (RuleEv Proposed) testRule (testGame {_rules=[testRule]})))  == Active

--testAutoDelete :: Rule
--testAutoDelete = do
--   outputAll_ "before"
--   autoDelete
--   outputAll_ "after"
--
--testAutoDeleteEx = isOutput "ok" (execRuleGame testAutoDelete testGame {_rules=[testRule {_rNumber = 1}]})

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

-- Test deletes
testDeleteRule :: Rule
testDeleteRule = do
    newVar_ "toto" (1::Int)
    onMessage (Msg "msg" :: Msg ()) (const $ return ())
    void $ newOutput (Just 1) (return "toto")

testDeleteGame :: Game
testDeleteGame = flip execState testGame {_players = []} $ runSystemEval 1 $ do
  addActivateRule testDeleteRule 1
  addActivateRule (void $ suppressRule 1) 2

testDeleteRuleEx1 :: Bool
testDeleteRuleEx1 = (_rStatus $ head $ drop 1 $ _rules testDeleteGame) == Reject &&
                    --(_variables testDeleteGame == []) &&
                    (_oStatus $ head $ _outputs testDeleteGame) == SDeleted &&
                    (_evStatus $ head $ _events testDeleteGame) == SDeleted

-- Test victory
testVictoryGame :: Game
testVictoryGame = flip execState testGame $ runSystemEval' $ do
  addActivateRule (victoryXRules 1) 1
  addActivateRule (nothing) 2

testVictoryEx1 :: Bool
testVictoryEx1 = (length $ getVictorious testVictoryGame) == 1

-- Test votes

voteGameActions :: Int -> Int -> Int  -> Bool -> Evaluate () -> Game
voteGameActions positives negatives total timeEvent actions = flip execState testGame {_players = []} $ runSystemEval' $ do
    mapM_ (\x -> addPlayer (PlayerInfo x ("coco " ++ show x) Nothing)) [1..total]
    actions
    evProposeRule testRule
    evs <- lift getChoiceEvents
    mapM_ (triggerVote 0) (take positives evs)                  --issuing positive votes
    mapM_ (triggerVote 1) (take negatives $ drop positives evs) --issuing negative votes
    when timeEvent $ evTriggerTime date2

--Trigger a vote event (0 for positive, 1 for negative), using event details
triggerVote :: Int -> (EventNumber, SignalAddress, PlayerNumber, String) -> Evaluate ()
triggerVote res (en, sa, pn, t) = triggerInput (RadioField pn t [(0,"For"),(1,"Against")]) (RadioData res) sa en

voteGame' :: Int -> Int -> Int -> Bool -> Rule -> Game
voteGame' positives negatives notVoted timeEvent rf  = voteGameActions positives negatives notVoted timeEvent $ addActivateRule rf 1

voteGame :: Int -> Int -> Int -> Rule -> Game
voteGame positives negatives notVoted = voteGame' positives negatives notVoted False

voteGameTimed :: Int -> Int -> Int -> Rule -> Game
voteGameTimed positives negatives notVoted = voteGame' positives negatives notVoted True

testVoteAssessOnVoteComplete1, testVoteAssessOnVoteComplete2, testVoteAssessOnEveryVote1, testVoteAssessOnEveryVote2, testVoteAssessOnEveryVote3, testVoteAssessOnEveryVote4, testVoteMajorityWith, testVoteNumberPositiveVotes, testVoteWithQuorum1, testVoteAssessOnTimeLimit1, testVoteAssessOnTimeLimit2, testVoteAssessOnTimeLimit3, testVoteAssessOnTimeLimit4, testVoteAssessOnTimeLimit5 :: Bool
-- vote rules                                |Expected result        |pos |neg |total           |description of voting system
testVoteAssessOnVoteComplete1 = testVoteRule Active  $ voteGame      10 0 10 $ onRuleProposed $ (callVoteRule majority oneDay)
testVoteAssessOnVoteComplete2 = testVoteRule Pending $ voteGame      9  0 10 $ onRuleProposed $ (callVoteRule unanimity oneDay)
testVoteAssessOnEveryVote1    = testVoteRule Active  $ voteGame      10 0 10 $ onRuleProposed $ (callVoteRule unanimity oneDay)
testVoteAssessOnEveryVote2    = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ (callVoteRule majority oneDay)
testVoteAssessOnEveryVote3    = testVoteRule Pending $ voteGame      5  0 10 $ onRuleProposed $ (callVoteRule majority oneDay)
testVoteAssessOnEveryVote4    = testVoteRule Reject  $ voteGame      0  5 10 $ onRuleProposed $ (callVoteRule majority oneDay)
testVoteMajorityWith          = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ (callVoteRule (majorityWith 50) oneDay)
testVoteNumberPositiveVotes   = testVoteRule Active  $ voteGame      3  7 10 $ onRuleProposed $ (callVoteRule (numberVotes 3) oneDay)
testVoteWithQuorum1           = testVoteRule Active  $ voteGame      7  3 10 $ onRuleProposed $ (callVoteRule (majority `withQuorum` 7) oneDay)
testVoteAssessOnTimeLimit1    = testVoteRule Active  $ voteGameTimed 10 0 10 $ onRuleProposed $ (callVoteRule' unanimity date2)
testVoteAssessOnTimeLimit2    = testVoteRule Active  $ voteGameTimed 1  0 10 $ onRuleProposed $ (callVoteRule' unanimity date2)
testVoteAssessOnTimeLimit3    = testVoteRule Reject  $ voteGameTimed 1  0 10 $ onRuleProposed $ (callVoteRule' (unanimity `withQuorum` 5) date2)
testVoteAssessOnTimeLimit4    = testVoteRule Reject  $ voteGameTimed 0  0 10 $ onRuleProposed $ (callVoteRule' (unanimity `withQuorum` 1) date2)
testVoteAssessOnTimeLimit5    = testVoteRule Active  $ voteGameTimed 1  0 10 $ onRuleProposed $ (callVoteRule' (unanimity `withQuorum` 1) date2)

testVoteRule s g = (_rStatus $ head $ _rules g) == s

--Test with a player arriving in the middle of a vote (he should be able to vote)
testVotePlayerArrive :: Game
testVotePlayerArrive = flip execState testGame {_players = []} $ runSystemEval' $ do
    addPlayer (PlayerInfo 1 "coco 1" Nothing)
    addActivateRule  (onRuleProposed $ callVoteRule (unanimity `withQuorum` 2) oneDay) 1
    evProposeRule testRule
    evs <- lift getChoiceEvents
    mapM_ (triggerVote 0) evs                 --issuing positive vote player 1
    addPlayer (PlayerInfo 2 "coco 2" Nothing) --new player
    evs <- lift getChoiceEvents
    mapM_ (triggerVote 0) evs                 --issuing positive vote player 2

testVotePlayerArriveEx = testVoteRule Active testVotePlayerArrive

--Test with a player leaving in the middle of a vote
--in some cases a player leaving can trigger the end of the vote (i.e when everybody voted except him)
testVotePlayerLeave :: Game
testVotePlayerLeave = flip execState testGame {_players = []} $ runSystemEval' $ do
    addPlayer (PlayerInfo 1 "coco 1" Nothing)
    addPlayer (PlayerInfo 2 "coco 2" Nothing)
    addActivateRule  (onRuleProposed $ callVoteRule unanimity oneDay) 1
    evProposeRule testRule
    evs <- lift getChoiceEvents
    triggerVote 0 (last evs)  -- issuing positive vote player 1
    evDelPlayer 2             -- player 2 leaving

testVotePlayerLeaveEx :: Bool
testVotePlayerLeaveEx = testVoteRule Active testVotePlayerLeave

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

addPlayer :: PlayerInfo -> Evaluate Bool
addPlayer pi = do
   pls <- use (eGame . players)
   let exists = any (((==) `on` _playerNumber) pi) pls
   unless exists $ do
       (eGame . players) %= (pi:)
       triggerEvent (Player Arrive) pi
   return $ not exists

isOutput :: String -> Game -> Bool
isOutput s g = s `elem` allOutputs g
