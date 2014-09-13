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
import Language.Nomyx.Outputs
import Language.Nomyx.Players
import Language.Nomyx.Rules
import Control.Monad.State hiding (forM_)
import Data.Maybe
import Data.Typeable
import Data.Time hiding (getCurrentTime)
import Control.Arrow
import Control.Applicative
import Control.Shortcut
import Data.List
import qualified Data.Map as M
import Debug.Trace

-- | a vote assessing function (such as unanimity, majority...)
type AssessFunction = VoteStats -> Maybe Bool

-- | the vote statistics, including the number of votes per choice,
-- the number of persons called to vote, and if the vote is finished (timeout or everybody voted)
data VoteStats = VoteStats { voteCounts     :: M.Map Bool Int,
                             nbParticipants :: Int,
                             voteFinished   :: Bool}
                             deriving (Show, Typeable)

-- | vote at unanimity every incoming rule
unanimityVote :: Nomex ()
unanimityVote = onRuleProposed $ callVoteRule unanimity oneDay

-- | call a vote on a rule for every players, with an assessing function and a delay
callVoteRule :: AssessFunction -> NominalDiffTime -> RuleInfo -> Nomex ()
callVoteRule assess delay ri = do
   endTime <- addUTCTime delay <$> liftEffect getCurrentTime
   callVoteRule' assess endTime ri

callVoteRule' :: AssessFunction -> UTCTime -> RuleInfo -> Nomex ()
callVoteRule' assess endTime ri = do
   let title = "Vote for rule: \"" ++ (_rName ri) ++ "\" (#" ++ (show $ _rNumber ri) ++ "):"
   callVote assess endTime title (finishVote assess ri)

-- | actions to do when the vote is finished
finishVote :: AssessFunction -> RuleInfo -> [(PlayerNumber, Maybe Bool)] -> Nomex ()
finishVote assess ri vs = do
       let passed = fromJust $ assess $ getVoteStats (map snd vs) True
       activateOrRejectRule ri passed
       void $ outputAll $ showFinishedVote (_rNumber ri) passed vs


-- | call a vote for every players, with an assessing function, a delay and a function to run on the result
callVote :: AssessFunction -> UTCTime -> String -> ([(PlayerNumber, Maybe Bool)] -> Nomex ()) -> Nomex ()
callVote assess endTime title payload = do
   en <- onEventOnce (voteWith endTime assess title) payload
   displayVote en

-- | vote with a function able to assess the ongoing votes.
-- | the vote can be concluded as soon as the result is known.
voteWith :: UTCTime -> AssessFunction -> String -> Event [(PlayerNumber, Maybe Bool)]
voteWith timeLimit assess title = do
   pns <- liftNomexNE getAllPlayerNumbers
   let voteEvents = map (singleVote title) pns
   let timerEvent = timeEvent timeLimit
   let isFinished votes timer = isJust $ assess $ getVoteStats votes timer
   (vs, _)<- shortcut2b voteEvents timerEvent isFinished
   return $ zip pns vs


-- trigger the display of a radio button choice on the player screen, yelding either True or False.
-- after the time limit, the value sent back is Nothing.
singleVote :: String -> PlayerNumber -> Event Bool
singleVote title pn = inputRadio pn title [(True, "For"), (False, "Against")]

-- | assess the vote results according to a unanimity
unanimity :: AssessFunction
unanimity voteStats = voteQuota (nbVoters voteStats) voteStats

-- | assess the vote results according to an absolute majority (half voters plus one)
majority :: AssessFunction
majority voteStats = voteQuota ((nbVoters voteStats) `div` 2 + 1) voteStats
                       
-- | assess the vote results according to a majority of x (in %)
majorityWith :: Int -> AssessFunction
majorityWith x voteStats = voteQuota ((nbVoters voteStats) * x `div` 100 + 1) voteStats

-- | assess the vote results according to a fixed number of positive votes
numberVotes :: Int -> AssessFunction
numberVotes x voteStats = voteQuota x voteStats

-- | adds a quorum to an assessing function
withQuorum :: AssessFunction -> Int -> AssessFunction
withQuorum f minNbVotes = \voteStats -> if (voted voteStats) >= minNbVotes
                                        then f voteStats
                                        else if voteFinished voteStats then Just False else Nothing

getVoteStats :: [Maybe Bool] -> Bool -> VoteStats
getVoteStats votes finished = VoteStats
   {voteCounts   = M.fromList $ counts (catMaybes votes),
    nbParticipants = length votes,
    voteFinished = finished}

counts :: (Eq a, Ord a) => [a] -> [(a, Int)]
counts as = map (head &&& length) (group $ sort as)

-- | Compute a result based on a quota of positive votes.
-- the result can be positive if the quota if reached, negative if the quota cannot be reached anymore at that point, or still pending.
voteQuota :: Int -> VoteStats -> Maybe Bool
voteQuota q voteStats
   | M.findWithDefault 0 True  vs >= q                       = Just True
   | M.findWithDefault 0 False vs > (nbVoters voteStats) - q = Just False
   | otherwise = Nothing
   where vs = voteCounts voteStats


-- | number of people that voted if the voting is finished,
-- total number of people that should vote otherwise
nbVoters :: VoteStats -> Int
nbVoters vs
   | voteFinished vs = voted vs
   | otherwise = nbParticipants vs

voted, notVoted :: VoteStats -> Int
notVoted    vs = (nbParticipants vs) - (voted vs)
voted       vs = M.findWithDefault 0 True (voteCounts vs) + M.findWithDefault 0 False (voteCounts vs)

displayVote :: EventNumber -> Nomex ()
displayVote en = void $ outputAll $ do
   mds <- getIntermediateResults en
   let mbs = map getBooleanResult <$> mds
   pns <- getAllPlayerNumbers
   case mbs of
      Just bs -> showOnGoingVote $ getVotes pns bs
      Nothing -> return ""

getVotes :: [PlayerNumber] -> [(PlayerNumber, Bool)] -> [(PlayerNumber, Maybe Bool)]
getVotes pns rs = map (findVote rs) pns where
   findVote :: [(PlayerNumber, Bool)] -> PlayerNumber -> (PlayerNumber, Maybe Bool)
   findVote rs pn = case (find (\(pn1, _) -> pn == pn1) rs) of
      Just (pn, b) -> (pn, Just b)
      Nothing -> (pn, Nothing)

getBooleanResult :: (PlayerNumber, SomeData) -> (PlayerNumber, Bool)
getBooleanResult (pn, SomeData sd) = case (cast sd) of
   Just a  -> (pn, a)
   Nothing -> error "incorrect vote field"


showOnGoingVote :: [(PlayerNumber, Maybe Bool)] -> NomexNE String
showOnGoingVote [] = return "Nobody voted yet"
showOnGoingVote listVotes = do
   list <- mapM showVote listVotes
   return $ "Votes:" ++ "\n" ++ concatMap (\(name, vote) -> name ++ "\t" ++ vote ++ "\n") list

showFinishedVote :: RuleNumber -> Bool -> [(PlayerNumber, Maybe Bool)] -> NomexNE String
showFinishedVote rn passed l = do
   let title = "Vote finished for rule #" ++ (show rn) ++ ", passed: " ++ (show passed)
   let voted = filter (\(_, r) -> isJust r) l
   votes <- mapM showVote voted
   return $ title ++ " (" ++ (intercalate ", " $ map (\(name, vote) -> name ++ ": " ++ vote) votes) ++ ")"

showVote :: (PlayerNumber, Maybe Bool) -> NomexNE (String, String)
showVote (pn, v) = do
   name <- showPlayer pn
   return (name, showChoice v)

showChoice :: Maybe Bool -> String
showChoice (Just a) = show a
showChoice Nothing  = "Not voted"
