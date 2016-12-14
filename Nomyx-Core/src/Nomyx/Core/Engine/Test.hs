{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Nomyx.Core.Engine.Test where

import Nomyx.Language.Types
import Nomyx.Language.Variables
import Nomyx.Language.Rules
import Nomyx.Language.Events
import Nomyx.Language.Outputs
import Nomyx.Language.Messages
import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.Types
import Nomyx.Library.Examples
import Nomyx.Library.Vote
import Nomyx.Library.Victory
import Control.Monad.State
import Data.Typeable
import Data.Function hiding ((.))
import Data.Maybe
import Control.Lens
import System.Random
import Imprevu.Test.TestMgt
import Imprevu.Evaluation hiding (events)
import Debug.NoTrace -- .Helpers    (traceM)


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
                       _rModules      = [],
                       _rRuleTemplate = RuleTemplate {_rName = "test",
                                                      _rDescription = "test",
                                                      _rRuleCode = "",
                                                      _rAuthor = "",
                                                      _rPicture = Nothing,
                                                      _rCategory = [],
                                                      _rDeclarations = []}}

execRuleGame :: Nomex a -> Game -> Game
execRuleGame r g = execState (runSystemEval' $ void $ evalNomex r) g

execRuleEventGame :: (Show e, Typeable e, Eq e, Eq s, Show s, Typeable s) => Nomex a -> Signal s e -> e -> Game -> Game
execRuleEventGame r f d g = execState (runSystemEval' $ evalNomex r >> (triggerEvent f d)) g

execRule :: Nomex a -> Game
execRule r = execRuleGame r testGame

addActivateRule :: Rule -> RuleNumber -> Evaluate ()
addActivateRule rf rn = do
   let rule = testRule & (rRuleTemplate . rName) .~ "testRule"
                       & rRule   .~ rf
                       & rNumber .~ rn
                       & rStatus .~ Pending
   evAddRule rule
   evActivateRule (_rNumber rule)
   return ()

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
   a <- readVar var
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
   a <- readVar var
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
   a <- readVar var
   case a of
      Just (a::[Int]) -> void $ writeVar var (2:a)
      Nothing         -> void $ newOutput (Just 1) (return "nok")

testVarEx5 :: Bool
testVarEx5 = (show $ head $ _variables $ execRule testVar5) == "Rule number = 0, Name = \"toto\", Value = [2,1]\n"

-- Test rule activation
testActivateRule :: Rule
testActivateRule = do
    a <- GetRules
    when (_rStatus (head a) == Pending) $ void $ ActivateRule $ _rNumber (head a)


testActivateRuleEx :: Bool
testActivateRuleEx = _rStatus (head $ _rules (execRuleGame testActivateRule testGame {_rules=[testRule]}))  == Active

testAutoActivateEx :: Bool
testAutoActivateEx = _rStatus (head $ _rules (execRuleEventGame autoActivate (Signal Proposed) testRule (testGame {_rules=[testRule]})))  == Active

--testAutoDelete :: Rule
--testAutoDelete = do
--   outputAll_ "before"
--   autoDelete
--   outputAll_ "after"
--
--testAutoDeleteEx = isOutput "ok" (execRuleGame testAutoDelete testGame {_rules=[testRule {_rNumber = 1}]})

-- Test deletes
testDeleteRule :: Rule
testDeleteRule = do
    newVar_ "toto" (1::Int)
    onMessage (Signal "msg" :: Msg ()) (const $ return ())
    void $ newOutput (Just 1) (return "toto")

testDeleteGame :: Game
testDeleteGame = flip execState testGame {_players = []} $ runSystemEval 1 $ do
  addActivateRule testDeleteRule 1
  addActivateRule (void $ suppressRule 1) 2

testDeleteRuleEx1 :: Bool
testDeleteRuleEx1 = (_rStatus $ head $ drop 1 $ _rules testDeleteGame) == Reject &&
                    --(_variables testDeleteGame == []) &&
                    (_oStatus $ head $ _outputs testDeleteGame) == SDeleted &&
                    (_evStatus $ _erEventInfo $ head $ _events testDeleteGame) == SDeleted

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
    traceM $  "choice events =" ++ (show evs)
    mapM_ (triggerVote 1) (take positives evs)                  --issuing positive votes
    mapM_ (triggerVote 0) (take negatives $ drop positives evs) --issuing negative votes
    when timeEvent $ evTriggerTime date2

--Trigger a vote event (0 for positive, 1 for negative), using event details
triggerVote :: Int -> Input -> Evaluate ()
triggerVote res is = triggerInput is (RadioData res)

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
    mapM_ (triggerVote 1) evs                 --issuing positive vote player 1
    addPlayer (PlayerInfo 2 "coco 2" Nothing) --new player
    evs <- lift getChoiceEvents
    mapM_ (triggerVote 1) evs                 --issuing positive vote player 2

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
    triggerVote 1 (last evs)  -- issuing positive vote player 1
    evDelPlayer 2             -- player 2 leaving

testVotePlayerLeaveEx :: Bool
testVotePlayerLeaveEx = testVoteRule Active testVotePlayerLeave


--Get all event numbers of type choice (radio button)
getChoiceEvents :: State EvalEnv [Input] 
getChoiceEvents = do
   evs <- use (evalEnv . eGame . events)
   ee <- get
   return $ concatMap (getInputChoices ee) evs

getInputChoices :: EvalEnv -> RuleEventInfo -> [Input]
getInputChoices ee (RuleEventInfo _ ei) = mapMaybe isInput (getRemainingSignals ei ee) where
   isInput :: SomeSignal -> Maybe Input
   isInput (SomeSignal (Signal s)) = cast s

addPlayer :: PlayerInfo -> Evaluate Bool
addPlayer pi = do
   pls <- use (evalEnv . eGame . players)
   let exists = any (((==) `on` _playerNumber) pi) pls
   unless exists $ do
       (evalEnv . eGame . players) %= (pi:)
       triggerEvent (Signal Arrive) pi
   return $ not exists

isOutput :: String -> Game -> Bool
isOutput s g = s `elem` allOutputs g
