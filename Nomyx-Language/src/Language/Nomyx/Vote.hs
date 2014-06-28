{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
    
-- | Voting system
module Language.Nomyx.Vote where

import Prelude hiding (foldr)
import Language.Nomyx.Expression
import Language.Nomyx.Events
import Language.Nomyx.Inputs
import Language.Nomyx.Players
import Language.Nomyx.Rules
import Control.Monad.State hiding (forM_)
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Control.Arrow
import Control.Applicative
import Data.List
import qualified Data.Map as M
import Debug.Trace (trace)

-- the type of a vote assessing function (such as unanimity, majority...)
type AssessFunction = VoteStats -> Maybe Bool

data VoteStats = VoteStats { voteCounts   :: M.Map (Maybe Bool) Int,
                             nbParticipants :: Int,
                             voteFinished :: Bool}
                             deriving (Show)

unanimityVoteRules :: Nomex ()
unanimityVoteRules = onRuleProposed $ (callVote unanimity oneDay) . activateOrRejectRule

callVote :: AssessFunction -> NominalDiffTime -> (Bool -> Nomex ()) -> Nomex ()
callVote f delay payload = do
   endTime <- addUTCTime delay <$> liftEffect getCurrentTime
   callVote' f endTime payload

callVote' :: AssessFunction -> UTCTime -> (Bool -> Nomex ()) -> Nomex ()
callVote' f endTime payload = do
   pns <- liftEffect getAllPlayerNumbers
   void $ onEventOnce (voteWith endTime pns f) payload

-- vote withe a function able to assess the ongoing votes.
-- the vote can be concluded as soon as the result is known.
voteWith :: UTCTime -> [PlayerNumber] -> AssessFunction -> Event Bool
voteWith timeLimit pns assess = shortcutEvents (voteEvents timeLimit pns) (assess . getVoteStats (length pns))

-- list of vote events
voteEvents :: UTCTime -> [PlayerNumber] -> [Event (Maybe Bool)]
voteEvents time pns = map (singleVote time) pns

-- trigger the display of a radio button choice on the player screen, yelding either Just True or Just False.
-- after the time limit, the value sent back is Nothing.
singleVote :: UTCTime -> PlayerNumber -> Event (Maybe Bool)
singleVote timeLimit pn = (Just <$> inputRadio pn "Vote for " [True, False] True) <|> (Nothing <$ timeEvent timeLimit)

unanimity :: AssessFunction
unanimity voteStats = voteQuota (nbVoters voteStats) voteStats

--atLeastOne :: AssessFunction
--atLeastOne npPlayers votes = if (length (filter (== Just True) votes) >= 1) then Just True else if (length votes == npPlayers) then Just False else Nothing

-- | assess the vote results according to an absolute majority (half voters plus one)
majority :: AssessFunction
majority voteStats = voteQuota ((nbVoters voteStats) `div` 2 + 1) voteStats
                       
-- | assess the vote results according to a majority of x (in %)
majorityWith :: Int -> AssessFunction
majorityWith x voteStats = voteQuota ((nbVoters voteStats) * x `div` 100 + 1) voteStats


nbPositives :: [Maybe Bool] -> Int
nbPositives votes = length $ filter (== Just True) votes

onRuleProposed :: (RuleInfo -> Nomex ()) -> Nomex ()
onRuleProposed f = void $ onEvent_ (ruleEvent Proposed) f


-- | adds a quorum to an assessing function
withQuorum :: AssessFunction -> Int -> AssessFunction
withQuorum f minNbVotes = \voteStats -> if (voted voteStats) >= minNbVotes
                                        then f voteStats
                                        else if voteFinished voteStats then Just False else Nothing

getVoteStats :: Int -> [Maybe Bool] -> VoteStats
getVoteStats npPlayers votes = VoteStats
   {voteCounts   = M.fromList $ counts votes,
    nbParticipants = npPlayers,
    voteFinished = length votes == npPlayers}

counts :: (Eq a, Ord a) => [a] -> [(a, Int)]
counts as = map (head &&& length) (group $ sort as)

-- |
voteQuota :: Int -> VoteStats -> Maybe Bool
voteQuota q voteStats
   | M.findWithDefault 0 (Just True)  vs >= q                       = Just True
   | M.findWithDefault 0 (Just False) vs > (nbVoters voteStats) - q = Just False
   | otherwise = Nothing
   where vs = voteCounts voteStats


-- | number of people that voted if the voting is finished,
-- total number of people that should vote otherwise
nbVoters :: VoteStats -> Int
nbVoters vs
   | voteFinished vs = (nbParticipants vs) - (notVoted vs)
   | otherwise = nbParticipants vs

totalVoters, voted, notVoted :: VoteStats -> Int
totalVoters vs = M.foldr (+) 0 (voteCounts vs)
notVoted    vs = fromMaybe 0 $ M.lookup Nothing (voteCounts vs)
voted       vs = (totalVoters vs) - (notVoted vs)
