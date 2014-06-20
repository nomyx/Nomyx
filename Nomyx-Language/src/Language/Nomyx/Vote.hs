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
import Language.Nomyx.Variables
import Language.Nomyx.Outputs
import Language.Nomyx.Inputs
import Language.Nomyx.Players
import Language.Nomyx.Rules
import Data.Typeable
import Control.Monad.State hiding (forM_)
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Data.Traversable hiding (mapM)
import Control.Arrow
import Control.Applicative
import Data.List
import Data.Function
import qualified Data.Map as M
import Control.Monad.Error (MonadError(..))



data VoteType a = ExclusiveVote (Maybe (Alts a))
                | NonExclusiveVote [Alts a]

class (Eq (Alts a), Show (Alts a), Ord (Alts a), Typeable a) => Votable a where
   data Alts a
   alts :: [Alts a]                                -- The alternatives
   quota :: Alts a -> Int -> Int -> Int            -- "quota alts q total" gives the quota to reach for each alternatives
   name :: a -> String
   exclusiveWinner :: a ->  Maybe (Alts a, Alts a) --The Votable as only two alternatives, one excluding the other
   exclusiveWinner _ = Nothing

type ForAgainst = Alts RuleInfo
instance Votable RuleInfo where
   data Alts RuleInfo = For | Against deriving (Typeable, Enum, Show, Eq, Bounded, Read, Ord)
   alts = [For, Against]
   quota For q _ = q
   quota Against q vs = vs - q + 1
   name r = "rule " ++ (show $ _rNumber r)
   exclusiveWinner _ = Just (For, Against)


type Vote a = (PlayerNumber, Maybe (Alts a))
type VoteResult a = VoteStats a -> [Alts a]
data VoteStats a = VoteStats { voteCounts :: M.Map (Maybe (Alts a)) Int,
                               voteFinished :: Bool}

data VoteData a = VoteData { msgEnd :: Msg [Alts a],                   -- message sent when the vote is finished with the winners
                             voteVar :: ArrayVar PlayerNumber (Alts a),-- variable containing the votes
                             inputNumbers :: [EventNumber],            -- numbers of the toggle inputs of the players (to delete them at the end of vote)
                             assessFunction :: VoteResult a,           -- function used to count the votes
                             timeLimit :: Maybe UTCTime}               -- end of the vote
                             
type Assessor a = StateT (VoteData a) Nomex ()

--TODO: decorelate effects and non effects
-- | Perform a vote.
voteWith :: (Votable a) => VoteResult a         -- ^ the function used to count the votes.
                        -> Assessor a           -- ^ assessors: when and how to perform the vote assessment (several assessors can be chained).
                        -> a                    -- ^ toVote: the matter to be voted.
                        -> [Alts a]             -- ^ the vote alternatives.
                        -> Nomex (Msg [Alts a]) -- ^ return value: a message containing the result of the vote. 
voteWith countVotes assessors toVote als = do
    pns <- liftEffect getAllPlayerNumbers
    let toVoteName = name toVote
    let msgEnd = Msg ("Result of votes for " ++ toVoteName) :: Msg [Alts a]
    --create an array variable to store the votes
    (voteVar :: ArrayVar PlayerNumber (Alts a)) <- newArrayVar_ ("Votes for " ++ toVoteName) pns
    --create the voting buttons
    let askPlayer pn = onInputRadioOnce ("Vote for " ++ toVoteName ++ ":") als (putArrayVar_ voteVar pn) pn
    inputs <- mapM askPlayer pns
    let voteData = VoteData msgEnd voteVar inputs countVotes Nothing
    --set the assessors
    evalStateT assessors voteData
    --display the vote
    displayVoteVar Nothing ("On-going vote for " ++ toVoteName ++ ":") voteVar
    displayVoteResult toVoteName voteData
    --clean the vote at the end
    cleanVote voteData
    return msgEnd

-- | Performs a vote, all the possible alternatives are selected.
voteWith_ :: (Votable a) => VoteResult a -> Assessor a -> a -> Nomex (Msg [Alts a]) 
voteWith_ assessFunction assessors toVote = voteWith assessFunction assessors toVote alts


-- | assess the vote on every new vote with the assess function, and as soon as the vote has an issue (positive of negative), sends a signal
assessOnEveryVote :: (Votable a) => Assessor a
assessOnEveryVote = do
   (VoteData msgEnd voteVar _ assess _) <- get
   lift $ void $ onMsgVarEvent voteVar $ f assess msgEnd where
      f assess msgEnd (VUpdated votes) = do
         let res = assess $ getVoteStats votes False
         unless (null res) $ sendMessage msgEnd res
      f _ _ _ = return ()   


-- | assess the vote with the assess function when time is reached, and sends a signal with the issue (positive of negative)
assessOnTimeLimit :: (Votable a) => UTCTime -> Assessor a
assessOnTimeLimit time = do
   (VoteData msgEnd voteVar i assess _) <- get
   put (VoteData msgEnd voteVar i assess (Just time))
   lift $ void $ onEvent_ (timeEvent time) $ \_ -> do
      votes <- getMsgVarData_ voteVar
      sendMessage msgEnd (assess $ getVoteStats votes True)

   
-- | assess the vote with the assess function when time is elapsed, and sends a signal with the issue (positive of negative)
assessOnTimeDelay :: (Votable a) => NominalDiffTime -> Assessor a
assessOnTimeDelay delay = do
   t <- addUTCTime delay <$> lift (liftEffect getCurrentTime)
   assessOnTimeLimit t

-- | assess the vote only when every body voted. An error is generated if the assessing function returns Nothing.
assessWhenEverybodyVoted :: (Votable a) => Assessor a
assessWhenEverybodyVoted = do
   (VoteData msgEnd voteVar _ assess _) <- get
   lift $ void $ onMsgVarEvent voteVar $ f assess msgEnd where
      f assess msgEnd (VUpdated votes) = when (all isJust (map snd votes)) $ sendMessage msgEnd $ assess $ getVoteStats votes True
      f _ _ _ = return ()

-- | clean events and variables necessary for the vote
cleanVote :: (Votable a) => VoteData a -> Nomex EventNumber
cleanVote (VoteData msgEnd voteVar inputsNumber _ _) = onMessageOnce msgEnd $ \_ -> do
   delMsgVar voteVar
   mapM_ delEvent inputsNumber

-- | a quorum is the neccessary number of voters for the validity of the vote
quorum :: (Votable a) => Int -> VoteStats a -> Bool
quorum q vs = (voted vs) >= q

-- | adds a quorum to an assessing function
withQuorum :: (Votable a) => VoteResult a -> Int -> VoteResult a
withQuorum assess q vs = if quorum q vs then assess vs else []

-- | assess the vote results according to a unanimity (everybody votes for)
unanimity :: (Votable a) => VoteStats a -> [Alts a]
unanimity vs = voteQuota (nbVoters vs) vs
  
-- | assess the vote results according to an absolute majority (half voters plus one, no quorum is needed)
majority :: (Votable a) => VoteStats a -> [Alts a]
majority vs = voteQuota ((nbVoters vs) `div` 2 + 1) vs

-- | assess the vote results according to a majority of x (in %)
majorityWith :: (Votable a) => Int -> VoteStats a -> [Alts a]
majorityWith x vs = voteQuota ((nbVoters vs) * x `div` 100 + 1) vs

-- | assess the vote results according to a necessary number of positive votes
numberVotes :: (Votable a) => Int -> VoteStats a -> [Alts a]
numberVotes = voteQuota

-- | the winners are the x vote alternatives with the more votes
firstXBest :: forall a. (Votable a) => Int -> VoteStats a -> [Alts a]
firstXBest x votes = map fst $ takeInGroups x sortedVotesWithExAequoGroups where
   voted (Just a, n) = Just (a, n)
   voted (Nothing, _) = Nothing
   sortedVotesWithExAequoGroups :: [[(Alts a, Int)]]
   sortedVotesWithExAequoGroups = groupBy ((==) `on` snd) $ sortWith snd $ mapMaybe voted $ M.assocs (voteCounts votes)

--take n elements from the first lists, but do not break groups
takeInGroups :: Int -> [[a]] -> [a]
takeInGroups 0 _ = []
takeInGroups n grps = if grpsSize <= n then head grps ++ takeInGroups (n - grpsSize) (tail grps) else head grps where
   grpsSize = length $ head grps

-- | the winner is the vote alternative with the more votes
firstBest :: (Votable a) => VoteStats a -> [Alts a]
firstBest = firstXBest 1

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

-- | return the vote alternatives that are above threshold
voteQuota :: forall a. (Votable a) => Int -> VoteStats a -> [Alts a]
voteQuota q votes = case exclusiveWinner (undefined :: a) of
   Nothing -> catMaybes $ M.keys $ M.filter (>= q) (voteCounts votes)
   Just a -> maybeToList $ exclusiveVoteQuota q votes a

-- | in case of exclusive winner
exclusiveVoteQuota :: (Votable a) => Int -> VoteStats a -> (Alts a, Alts a) -> Maybe (Alts a)
exclusiveVoteQuota q votes (for, against)
   | M.findWithDefault 0 (Just for) vs     >= q                   = Just for
   | M.findWithDefault 0 (Just against) vs > (nbVoters votes) - q = Just against
   | otherwise = Nothing
   where vs = voteCounts votes


-- | number of people that voted if the voting is finished,
-- total number of people that should vote otherwise
nbVoters :: (Votable a) => VoteStats a -> Int
nbVoters vs
   | voteFinished vs = (totalVoters vs) - (notVoted vs)
   | otherwise = totalVoters vs

totalVoters, voted, notVoted :: (Votable a) => VoteStats a -> Int
totalVoters vs = M.foldr (+) 0 (voteCounts vs)
notVoted    vs = fromMaybe 0 $ M.lookup Nothing (voteCounts vs)
voted       vs = (totalVoters vs) - (notVoted vs)


getVoteStats :: (Votable a) => [Vote a] -> Bool -> VoteStats a
getVoteStats vs voteFinished = VoteStats
   {voteCounts = M.fromList $ counts (snd <$> vs),
    voteFinished = voteFinished}

counts :: (Eq a, Ord a) => [a] -> [(a, Int)]
counts as = map (head &&& length) (group $ sort as)


displayVoteVar :: (Votable a) => (Maybe PlayerNumber) -> String -> ArrayVar PlayerNumber (Alts a) -> Nomex EventNumber
displayVoteVar mpn title mv = displayVar' mpn mv (showOnGoingVote title)

showChoice :: (Votable a) => Maybe (Alts a) -> String
showChoice (Just a) = show a
showChoice Nothing  = "Not Voted"

showChoices :: (Votable a) => [Alts a] -> String
showChoices [] = "no result"
showChoices cs = intercalate ", " $ map show cs

showOnGoingVote :: (Votable a) => String -> [(PlayerNumber, Maybe (Alts a))] -> NomexNE String
showOnGoingVote title listVotes = do
   list <- mapM showVote listVotes
   return $ title ++ "\n" ++ concatMap (\(name, vote) -> name ++ "\t" ++ vote ++ "\n") list

showFinishedVote :: (Votable a) =>  [(PlayerNumber, Maybe (Alts a))] -> NomexNE String
showFinishedVote l = do
   let voted = filter (\(_, r) -> isJust r) l
   votes <- mapM showVote voted
   return $ intercalate ", " $ map (\(name, vote) -> name ++ ": " ++ vote) votes

showVote :: (Votable a) => (PlayerNumber, Maybe (Alts a)) -> NomexNE (String, String)
showVote (pn, v) = do
   name <- showPlayer pn
   return (name, showChoice v)
                                              
displayVoteResult :: (Votable a) => String -> VoteData a -> Nomex OutputNumber
displayVoteResult toVoteName (VoteData msgEnd voteVar _ _ _) = onMessage msgEnd $ \result -> do
   vs <- getMsgVarData_ voteVar
   votes <- liftEffect $ showFinishedVote vs
   void $ outputAll_ $ "Vote result for " ++ toVoteName ++ ": " ++ showChoices result ++ " (" ++ votes ++ ")"

-- | any new rule will be activate if the rule in parameter returns For
onRuleProposed :: (RuleInfo -> Nomex (Msg [ForAgainst]) ) -> Rule
onRuleProposed f = void $ onEvent_ (ruleEvent Proposed) $ \rule -> do
    resp <- f rule
    void $ onMessageOnce resp $ (activateOrReject rule) . (== [For])

-- * Referendum & elections

data Referendum = Referendum String deriving (Typeable)
instance Votable Referendum where
   data Alts Referendum = Yes | No deriving (Enum, Show, Eq, Bounded, Read, Ord)
   alts = [Yes, No]
   quota Yes q _ = q
   quota No q vs = vs - q + 1
   name (Referendum n) = "referendum on " ++ n

referendum :: String -> Nomex () -> Rule
referendum name action = do
   msg <- voteWith_ (majority `withQuorum` 2) (assessOnEveryVote >> assessOnTimeDelay oneDay) (Referendum name)
   void $ onMessageOnce msg resolution where
      resolution [Yes] = do
            outputAll_ "Positive result of referendum"
            action
      resolution [No] = void $ outputAll_ "Negative result of referendum"
      resolution []   = void $ outputAll_ "No result for referendum"
      resolution _    = throwError "Impossible result for referendum"


data Election = Election String deriving (Typeable)
instance Votable Election where
   data Alts Election = Candidate {candidate :: PlayerInfo}
   alts = map (\n -> Candidate (PlayerInfo n "" Nothing)) [1..]
   quota _ q _ = q
   name (Election n) = "elections on " ++ n

instance Show (Alts Election) where
   show (Candidate (PlayerInfo _ name _)) = name

instance Eq (Alts Election) where
   (Candidate (PlayerInfo pn1 _ _)) == (Candidate (PlayerInfo pn2 _ _)) = pn1 == pn2

instance Ord (Alts Election) where
   compare (Candidate (PlayerInfo pn1 _ _)) (Candidate (PlayerInfo pn2 _ _)) = compare pn1 pn2

elections :: String -> [PlayerInfo] -> (PlayerNumber -> Nomex ()) -> Nomex ()
elections name pns action = do
   msg <- voteWith majority (assessOnTimeDelay oneDay) (Election name) (Candidate <$> pns)
   void $ onMessageOnce msg resolution where
      resolution [Candidate pi] = do
         outputAll_ $ "Result of elections: player(s) " ++ (show $ _playerName pi) ++ " won!"
         action $ _playerNumber pi
      resolution [] = void $ outputAll_ "Result of elections: nobody won!"
      resolution _  = throwError "Impossible result for elections"

voteEvent :: UTCTime -> [PlayerNumber] -> Event ([Maybe Bool])
voteEvent time pns = sequenceA $ map (singleVote time) pns

singleVote :: UTCTime -> PlayerNumber -> Event (Maybe Bool)
singleVote timeLimit pn = (Just <$> inputRadio pn "Vote for "[True, False] True) <|> (Nothing <$ timeEvent timeLimit)

vote :: UTCTime -> [PlayerNumber] -> Event Bool
vote timeLimit pns = unanimity' <$> (voteEvent timeLimit pns)


unanimity' :: [Maybe Bool] -> Bool
unanimity' = all (== Just True)

callVote :: UTCTime -> Nomex ()
callVote t = do
   pns <- liftEffect getAllPlayerNumbers
   void $ onEventOnce (vote' t pns) (outputAll_ . show)

voteEvent' :: UTCTime -> [PlayerNumber] -> [Event (Maybe Bool)]
voteEvent' time pns = map (singleVote time) pns

vote' :: UTCTime -> [PlayerNumber] -> Event Bool
vote' timeLimit pns = shortcutEvents (voteEvent' timeLimit pns) (\(as :: [Maybe Bool]) -> atLeastOne (length pns) as)

unanimity'' :: Int -> [Maybe Bool] -> Maybe Bool
unanimity'' npPlayers res = if (length res == npPlayers) then Just $ all (== Just True) res else Nothing

atLeastOne :: Int -> [Maybe Bool] -> Maybe Bool
atLeastOne npPlayers res = if (length (filter (== Just True) res) >= 1) then Just True else if (length res == npPlayers) then Just False else Nothing

