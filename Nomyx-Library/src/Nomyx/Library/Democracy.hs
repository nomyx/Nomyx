
-- | This file gives a list of example rules that the players can submit.
module Nomyx.Library.Democracy where

import Language.Nomyx
import Nomyx.Library.Vote


-- | a majority vote, with the folowing parameters:
-- a quorum of 2 voters is necessary for the validity of the vote
-- the vote is assessed after every vote in case the winner is already known
-- the vote will finish anyway after one day
voteWithMajority :: Rule
voteWithMajority = onRuleProposed $ callVoteRule (majority `withQuorum` 2) oneDay

-- | Change current system (the rules passed in parameter) to absolute majority (half participants plus one)
democracy :: [RuleNumber] -> Rule
democracy rs = do
   mapM_ suppressRule rs
   rNum <- addRule' "vote with majority" voteWithMajority "voteWithMajority" "majority with a quorum of 2"
   activateRule_ rNum
   autoDelete

