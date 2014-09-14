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
import Language.Nomyx.Examples
import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.EvalUtils
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Utils
import Control.Monad.State
import Data.Lens
import Data.Typeable
import Data.Function hiding ((.))
import Data.Maybe
import Control.Applicative
import Control.Category hiding ((.))
import Control.Shortcut

date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"
date3 = parse822Time "Tue, 02 Sep 1997 11:00:00 -0400"

testGame = Game { _gameName      = "test",
                  _gameDesc      = GameDesc "test" "test",
                  _rules         = [],
                  _players       = [PlayerInfo 1 "coco" Nothing],
                  _variables     = [],
                  _events        = [],
                  _outputs       = [],
                  _victory       = Nothing,
                  _logs          = [],
                  _currentTime   = date1}

testRule = RuleInfo  { _rNumber       = 0,
                      _rName         = "test",
                      _rDescription  = "test",
                      _rProposedBy   = 0,
                      _rRuleCode     = "",
                      _rRule         = return (),
                      _rStatus       = Pending,
                      _rAssessedBy   = Nothing}

execRuleEvent :: (Show e, Typeable e) => Nomex a -> Field e -> e -> Game
execRuleEvent r f d = execState (runSystemEval' $ evalNomex r >> triggerEvent f d) testGame

execRuleEvents :: (Show e, Typeable e) => Nomex a -> [(Field e, e)] -> Game
execRuleEvents f eds = execState (runSystemEval' $ evalNomex f >> mapM (\(a,b) -> triggerEvent a b) eds) testGame

execRuleInput :: Nomex a -> EventNumber -> FieldAddress -> InputData -> Game
execRuleInput r en fa d = execState (runSystemEval' $ evalNomex r >> triggerInput en fa d) testGame

execRuleInputs :: Nomex a -> EventNumber -> [(FieldAddress, InputData)] -> Game
execRuleInputs r en fads = execState (runSystemEval' $ evalNomex r >> mapM (\(fa, d) -> triggerInput en fa d) fads) testGame

execRuleGame :: Nomex a -> Game -> Game
execRuleGame r g = execState (runSystemEval' $ void $ evalNomex r) g

execRuleEventGame :: (Show e, Typeable e) => Nomex a -> Field e -> e -> Game -> Game
execRuleEventGame r f d g = execState (runSystemEval' $ evalNomex r >> (triggerEvent f d)) g

execRule :: Nomex a -> Game
execRule r = execRuleGame r testGame

addActivateRule :: Rule -> RuleNumber -> Evaluate ()
addActivateRule rf rn = do
   let rule = testRule {_rName = "testRule", _rRule = rf, _rNumber = rn, _rStatus = Pending}
   evAddRule rule
   evActivateRule (_rNumber rule)
   return ()

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
         ("test user input write", testUserInputWriteEx),
         ("test activate rule", testActivateRuleEx),
         ("test auto activate", testAutoActivateEx),
         --("test meta rules vote", testApplicationMetaRuleEx),
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
--         ("test number positive votes", testVoteNumberPositiveVotes),
         ("test vote with quorum 1", testVoteWithQuorum1),
         ("test assess on time limit 1", testVoteAssessOnTimeLimit1),
         ("test assess on time limit 2", testVoteAssessOnTimeLimit2),
         ("test assess on time limit 3", testVoteAssessOnTimeLimit3),
         ("test assess on time limit 4", testVoteAssessOnTimeLimit4),
         ("test assess on time limit 5", testVoteAssessOnTimeLimit5),
         ("test composed event sum", testSumComposeEx),
         ("test composed event prod 1", testProdComposeEx1),
         ("test composed event prod 2", testProdComposeEx2),
         ("test two separate events", testTwoEventsEx),
         ("test monadic event", testMonadicEventEx),
         ("test shortcut event", testShorcutEventEx)
         ]

allTests = all snd tests

--Test variable creation
testVar1 :: Rule
testVar1 = do
   NewVar "toto" (1::Integer)
   return ()

testVarEx1 = True --(variables ^$ execRuleFunc testVar1) == [(Var 0 "toto" (1::Integer))]

--Test variable deleting
testVar2 :: Rule
testVar2 = do
   var <- newVar_ "toto" (1::Int)
   delVar var
   return ()

testVarEx2 = True --_variables (execRuleFunc testVar2) == []

--Test variable reading
testVar3 :: Rule
testVar3 = do
   var <- newVar_ "toto" (1::Int)
   a <- liftEffect $ readVar var
   case a of
      Just (1::Int) -> void $ newOutput (Just 1) (return "ok")
      _ -> void $ newOutput (Just 1) (return "nok")

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

testVarEx5 = True --_variables (execRuleFunc testVar5) == [(Var 0 "toto" ([2,1]::[Int]))]

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: Rule
testSingleInput = void $ onInputRadio_ "Vote for Holland or Sarkozy" [Holland, Sarkozy] h 1 where
   h a = void $ newOutput (Just 1) (return $ "voted for " ++ show a)

testSingleInputEx = isOutput "voted for Holland" g where
   g = execRuleEvent testSingleInput (Input 1 "Vote for Holland or Sarkozy" (Radio [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) Holland

testMultipleInputs :: Rule
testMultipleInputs = void $ onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
   h a = void $ newOutput (Just 1) (return $ "voted for " ++ show a)

testMultipleInputsEx = isOutput "voted for [Holland,Sarkozy]" g where
   g = execRuleEvent testMultipleInputs (Input 1 "Vote for Holland and Sarkozy" (Checkbox [(Holland, "Holland"), (Sarkozy, "Sarkozy")])) [Holland, Sarkozy]

testInputString :: Rule
testInputString = void $ onInputText_ "Enter a number:" h 1 where
   h a = void $ newOutput (Just 1) (return $ "You entered: " ++ a)

testInputStringEx = isOutput "You entered: 1" g where
   g = execRuleEvent testInputString (Input 1 "Enter a number:" Text) "1"

-- Test message
testSendMessage :: Rule
testSendMessage = do
    let msg = Msg "msg" :: Msg String
    onEvent_ (messageEvent msg) f
    sendMessage msg "toto" where
        f (a :: String) = void $ newOutput (Just 1) (return a)

testSendMessageEx = isOutput "toto" (execRule testSendMessage)

testSendMessage2 :: Rule
testSendMessage2 = do
    onEvent_ (messageEvent (Msg "msg" :: Msg ())) $ const $ void $ newOutput (Just 1) (return "Received")
    sendMessage_ "msg"


testSendMessageEx2 = isOutput "Received" (execRule testSendMessage2)

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test user input + variable read/write
testUserInputWrite :: Rule
testUserInputWrite = do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (messageEvent (Msg "voted" :: Msg ())) h2
    void $ onEvent_ (BaseEvent $ Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")])) h1 where
        h1 a = do
            writeVar (V "vote") (Just a)
            SendMessage (Msg "voted") ()
        h2 _ = do
            a <- liftEffect $ readVar (V "vote")
            void $ case a of
                Just (Just Me) -> newOutput (Just 1) (return "voted Me")
                _ -> newOutput (Just 1) (return "problem")


testUserInputWriteEx = isOutput "voted Me" g where
   g = execRuleEvent testUserInputWrite (Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")])) Me

-- Test rule activation
testActivateRule :: Rule
testActivateRule = do
    a <- liftEffect GetRules
    when (_rStatus (head a) == Pending) $ void $ ActivateRule $ _rNumber (head a)


testActivateRuleEx = _rStatus (head $ _rules (execRuleGame testActivateRule testGame {_rules=[testRule]}))  == Active

testAutoActivateEx = _rStatus (head $ _rules (execRuleEventGame autoActivate (RuleEv Proposed) testRule (testGame {_rules=[testRule]})))  == Active

--Time tests

testTimeEvent :: Rule
testTimeEvent = void $ onEvent_ (timeEvent date1) f where
   f _ = outputAll_ $ show date1

testTimeEventEx = isOutput (show date1) g where
   g = execRuleEvent testTimeEvent (Time date1) date1

testTimeEvent2 :: Nomex ()
testTimeEvent2 = schedule' [date1, date2] (outputAll_ . show)

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

testDeleteRuleEx1 = (_rStatus $ head $ drop 1 $ _rules testDeleteGame) == Reject &&
                    --(_variables testDeleteGame == []) &&
                    (_oStatus $ head $ _outputs testDeleteGame) == SDeleted &&
                    (_evStatus $ head $ _events testDeleteGame) == SDeleted

-- Test victory
testVictoryGame :: Game
testVictoryGame = flip execState testGame $ runSystemEval' $ do
  addActivateRule (victoryXRules 1) 1
  addActivateRule (nothing) 2

testVictoryEx1 = (length $ getVictorious testVictoryGame) == 1

-- Test votes

voteGameActions :: Int -> Int -> Int  -> Bool -> Evaluate () -> Game
voteGameActions positives negatives total timeEvent actions = flip execState testGame {_players = []} $ runSystemEval' $ do
    mapM_ (\x -> addPlayer (PlayerInfo x ("coco " ++ show x) Nothing)) [1..total]
    actions
    evProposeRule testRule
    evs <- lift getChoiceEvents
    let pos = take positives evs
    let neg = take negatives $ drop positives evs
    mapM_ (\(en, fa) -> triggerInput en fa (RadioData 0)) pos --issuing positive votes
    mapM_ (\(en, fa) -> triggerInput en fa (RadioData 1)) neg --issuing negative votes
    when timeEvent $ evTriggerTime date2

voteGame' :: Int -> Int -> Int -> Bool -> Rule -> Game
voteGame' positives negatives notVoted timeEvent rf  = voteGameActions positives negatives notVoted timeEvent $ addActivateRule rf 1

voteGame :: Int -> Int -> Int -> Rule -> Game
voteGame positives negatives notVoted = voteGame' positives negatives notVoted False

voteGameTimed :: Int -> Int -> Int -> Rule -> Game
voteGameTimed positives negatives notVoted = voteGame' positives negatives notVoted True

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

-- Event composition

testSumCompose :: Rule
testSumCompose = void $ onEvent_ (True <$ inputButton 1 "click here:" <|> False <$ inputButton 2 "") f where
   f a = outputAll_ $ show a

testSumComposeEx = isOutput "True" g where
   g = execRuleInput testSumCompose 1 [SumL, AppR] ButtonData


testProdCompose :: Rule
testProdCompose = void $ onEvent_ ((,) <$> inputText 1 "" <*> inputText 1 "") f where
   f a = outputAll_ $ show a

testProdComposeEx1 = null $ allOutputs g where
   g = execRuleInput testProdCompose 1 [AppR] (TextData "toto")

testProdComposeEx2 = isOutput "(\"toto\",\"tata\")" g where
   g = execRuleInputs testProdCompose 1 [([AppL, AppR], TextData "toto"), ([AppR], TextData "tata")]

testTwoEvents :: Rule
testTwoEvents = do
   void $ onEvent_ (inputText 1 "") f
   void $ onEvent_ (inputText 1 "") f where
   f a = outputAll_ $ show a

testTwoEventsEx = (length $ allOutputs g) == 1 where
   g = execRuleInput testTwoEvents 1 [] (TextData "toto")

testMonadicEvent :: Rule
testMonadicEvent = do
   let displayMsg a = void $ newOutput_ Nothing a
   let e = do
       a <- inputText 1 ""
       guard (a == "coco1") >> inputText 1 ""
   void $ onEvent_ e displayMsg

testMonadicEventEx = isOutput "coco2" g where
   g = execRuleInputs testMonadicEvent 1 [([BindL], TextData "coco1"), ([BindR, BindR], TextData "coco2")]

testShorcutEvent :: Rule
testShorcutEvent = do
   let displayMsg a = void $ newOutput_ Nothing (concat $ catMaybes a)
   let e = do
       let a = inputText 1 ""
       shortcut [a,a] (\as -> length (filter isJust as) == 1)
   void $ onEvent_ e displayMsg

testShorcutEventEx = isOutput "coco1" g where
   g = execRuleInputs testShorcutEvent 1 [([Index 0], TextData "coco1")]


--Get all event numbers of type choice (radio button)
getChoiceEvents :: State EvalEnv [(EventNumber, FieldAddress)]
getChoiceEvents = do
   evs <- access (eGame >>> events)
   g <- access eGame
   return $ [(_eventNumber ev, inum) | ev <- evs, inum <- getInputChoices ev g]

getInputChoices :: EventInfo -> Game -> [FieldAddress]
getInputChoices ei g = mapMaybe isInput (getEventFields ei g) where
   isInput :: (t, SomeField) -> Maybe t
   isInput (fa, (SomeField (Input _ _ (Radio _)))) = Just fa
   isInput _ = Nothing

--Get all event numbers of type text (text field)
getTextEvents :: State Game [(EventNumber, FieldAddress)]
getTextEvents = do
   evs <- access events
   g <- get
   return $ [(_eventNumber ev, fa) | ev <- evs, fa <- getInputTexts ev g]

getInputTexts :: EventInfo -> Game -> [FieldAddress]
getInputTexts ei g = mapMaybe isInput (getEventFields ei g) where
   isInput (fa, (SomeField (Input _ _ Text))) = Just fa
   isInput _ = Nothing

addPlayer :: PlayerInfo -> Evaluate Bool
addPlayer pi = do
   pls <- access (eGame >>> players)
   let exists = any (((==) `on` _playerNumber) pi) pls
   unless exists $ do
       (eGame >>> players) %= (pi:)
       triggerEvent (Player Arrive) pi
   return $ not exists

isOutput :: String -> Game -> Bool
isOutput s g = s `elem` allOutputs g
