{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module manages multi-player games.
module Nomyx.Core.Multi where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State
import           Data.List
import           Data.Time                           as T
import           Language.Haskell.Interpreter.Server (ServerHandle)
import           Language.Nomyx
import           Nomyx.Core.Engine                   as G
import           Nomyx.Core.Interpret
import           Nomyx.Core.Quotes                   (cr)
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           System.Random

triggerTimeEvent :: UTCTime -> StateT Multi IO ()
triggerTimeEvent t = do
   gs <- use gameInfos
   gs' <- lift $ mapM (trig' t) gs
   gameInfos .= gs'

trig' :: UTCTime -> GameInfo -> IO GameInfo
trig' t gi = do
   lg <- trig t (_loggedGame gi)
   return $ gi {_loggedGame = lg}

trig :: UTCTime -> LoggedGame -> IO LoggedGame
trig t g = do
   g' <- execWithGame' t (execGameEvent $ TimeEvent t) g
   evaluate g'

-- | get all events that has not been triggered yet
getTimeEvents :: UTCTime -> Multi -> IO [UTCTime]
getTimeEvents now m = do
   let games = map (_game . _loggedGame) (_gameInfos m)
   let times = concatMap getGameTimes games
   return $ filter (\t -> t <= now && t > (-32) `addUTCTime` now) times

-- | the initial rule set for a game.
rVoteUnanimity :: RuleDetails
rVoteUnanimity = RuleDetails "Unanimity Vote"
                            "A proposed rule will be activated if all players vote for it"
                            [cr|do
   onRuleProposed $ callVoteRule unanimity oneDay
   displayVotes|]
                            "Kau"
                            (Just "democracy.png")
                            []

rVictory5Rules :: RuleDetails
rVictory5Rules = RuleDetails "Victory 5 accepted rules"
                            "Victory is achieved if you have 5 active rules"
                            [cr|victoryXRules 5|]
                            "Kau"
                            Nothing
                            []

rVoteMajority :: RuleDetails
rVoteMajority = RuleDetails "Majority Vote"
                            "A proposed rule will be activated if a majority of players is reached, with a minimum of 2 players, and within oone day"
                            [cr|onRuleProposed $ callVoteRule (majority `withQuorum` 2) oneDay|]
                            "Kau"
                            Nothing
                            []


initialGame :: ServerHandle -> StateT GameInfo IO ()
initialGame sh = zoom loggedGame $ mapM_ addR [rVoteUnanimity, rVictory5Rules]
   where addR r = execGameEvent' (Just $ getRuleFunc sh) (SystemAddRule r)

initialGameInfo :: GameName -> GameDesc -> Bool -> Maybe PlayerNumber -> UTCTime -> ServerHandle -> IO GameInfo
initialGameInfo name desc isPub mpn date sh = do
   let gen = mkStdGen 0
   let lg = GameInfo { _loggedGame = LoggedGame (emptyGame name desc date gen) [],
                       _ownedBy    = mpn,
                       _forkedFromGame = Nothing,
                       _isPublic = isPub,
                       _startedAt  = date}

   execStateT (initialGame sh) lg

defaultMulti :: Settings -> Multi
defaultMulti s = Multi [] s [rVoteUnanimity, rVoteMajority, rVictory5Rules]

-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: GameInfo -> StateT Multi IO ()
modifyGame gi = do
   gs <- use gameInfos
   case find (== gi) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg gi gs
         gameInfos .= newgs

execWithMulti :: UTCTime -> StateT Multi IO () -> Multi -> IO Multi
execWithMulti t ms m = do
   let setTime g = (loggedGame . game . currentTime) .~ t $ g
   let m' = over gameInfos (map setTime) m
   execStateT ms m'
