module Test where

import Nomyx.Core.Engine.Test
import Distribution.TestSuite

tests :: IO [Test]
tests = return [
  test testVarEx1                    "test var 1",
  test testVarEx2                    "test var 2",
  test testVarEx3                    "test var 3",
  test testVarEx4                    "test var 4",
  test testVarEx5                    "test var 5",
  test testActivateRuleEx            "test activate rule",
  test testAutoActivateEx            "test auto activate",
  test testDeleteRuleEx1             "test delete rule",
  test testVictoryEx1                "test victory rule",
  test testVoteAssessOnVoteComplete1 "test assess on vote complete 1",
  test testVoteAssessOnVoteComplete2 "test assess on vote complete 2",
  test testVoteAssessOnEveryVote1    "test assess on every vote 1",
  test testVoteAssessOnEveryVote2    "test assess on every vote 2",
  test testVoteAssessOnEveryVote3    "test assess on every vote 3",
  test testVoteAssessOnEveryVote4    "test assess on every vote 4",
  test testVoteMajorityWith          "test majority with",
  test testVoteNumberPositiveVotes   "test number positive votes",
  test testVoteWithQuorum1           "test vote with quorum 1",
  test testVoteAssessOnTimeLimit1    "test assess on time limit 1",
  test testVoteAssessOnTimeLimit2    "test assess on time limit 2",
  test testVoteAssessOnTimeLimit3    "test assess on time limit 3",
  test testVoteAssessOnTimeLimit4    "test assess on time limit 4",
  test testVoteAssessOnTimeLimit5    "test assess on time limit 5",
  test testVotePlayerArriveEx        "test vote player arrives",
  test testVotePlayerLeaveEx         "test vote player leaves"]

test :: Bool -> String -> Test
test p t = Test $ test' p t

test' :: Bool -> String -> TestInstance
test' p t = TestInstance
   { run = return $ Finished $ if p then Pass else Fail ""
   , name = t
   , tags = []
   , options = []
   , setOption = \_ _ -> Right $ test' p t
   }

