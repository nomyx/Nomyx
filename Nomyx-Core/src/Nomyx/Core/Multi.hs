{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module manages multi-player games.
module Nomyx.Core.Multi where

import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Lens
import Data.List
import Data.Time as T
import Control.Exception
import Control.Monad.State
import Control.Applicative
import Control.Category hiding ((.))
import Language.Nomyx
import Nomyx.Core.Engine as G
import Nomyx.Core.Utils
import Nomyx.Core.Types
import Nomyx.Core.Interpret
import Nomyx.Core.Quotes (cr)
import System.Random

triggerTimeEvent :: UTCTime -> StateT Multi IO ()
triggerTimeEvent t = do
   gs <- access gameInfos
   gs' <- lift $ mapM (trig' t) gs
   void $ gameInfos ~= gs'

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
rVoteUnanimity = SubmitRule "Unanimity Vote"
                            "A proposed rule will be activated if all players vote for it"
                            [cr|onRuleProposed $ callVoteRule unanimity oneDay|]

rVictory5Rules = SubmitRule "Victory 5 accepted rules"
                            "Victory is achieved if you have 5 active rules"
                            [cr|victoryXRules 5|]

rVoteMajority = SubmitRule "Majority Vote"
                            "A proposed rule will be activated if a majority of players is reached, with a minimum of 2 players, and within oone day"
                            [cr|onRuleProposed $ callVoteRule (majority `withQuorum` 2) oneDay|]


initialGame :: ServerHandle -> StateT GameInfo IO ()
initialGame sh = focus loggedGame $ mapM_ addR [rVoteUnanimity, rVictory5Rules]
   where addR r = execGameEvent' (Just $ getRuleFunc sh) (SystemAddRule r)

initialGameInfo :: GameName -> GameDesc -> Bool -> Maybe PlayerNumber -> UTCTime -> ServerHandle -> IO GameInfo
initialGameInfo name desc isPublic mpn date sh = do
   let gen = mkStdGen 0
   let lg = GameInfo { _loggedGame = LoggedGame (emptyGame name desc date gen) [],
                       _ownedBy    = mpn,
                       _forkedFromGame = Nothing,
                       _isPublic = isPublic,
                       _startedAt  = date}

   execStateT (initialGame sh) lg

defaultMulti :: Settings -> Multi
defaultMulti = Multi []

-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: GameInfo -> StateT Multi IO ()
modifyGame gi = do
   gs <- access gameInfos
   case find (== gi) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg gi gs
         gameInfos ~= newgs
         return ()

execWithMulti :: UTCTime -> StateT Multi IO () -> Multi -> IO Multi
execWithMulti t ms m = do
   let setTime g = (loggedGame >>> game >>> currentTime) ^= t $ g
   let m' = gameInfos `modL` (map setTime) $ m
   execStateT ms m'
