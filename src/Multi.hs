{-# LANGUAGE QuasiQuotes #-}

-- | This module manages multi-player games and commands.
module Multi where

import Prelude
import Control.Monad.State
import Interpret
import Data.Time as T
import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Maybe
import Types
import Control.Exception
import Data.Lens
import Language.Nomyx.Engine as G
import Control.Category hiding ((.))
import Quotes (cr)

triggerTimeEvent :: UTCTime -> StateT Multi IO ()
triggerTimeEvent t = do
   gs <- access games
   gs' <- lift $ mapM (\g -> trig t g) gs
   void $ games ~= gs'


trig :: UTCTime -> LoggedGame -> IO LoggedGame
trig t g =  do
   g' <- execWithGame' t (execGameEvent $ TimeEvent t) g
   evaluate g'

-- | get all events that has not been triggered yet
getTimeEvents :: UTCTime -> Multi -> IO([UTCTime])
getTimeEvents now m = do
    let times = catMaybes $ map getTimes $ (concatMap (getL $ game >>> events)) $ _games  m
    return $ filter (\t -> t <= now && t > (-2) `addUTCTime` now) times

-- | the initial rule set for a game.
rVoteUnanimity = SubmitRule "Unanimity Vote"
                            "A proposed rule will be activated if all players vote for it"
                            [cr|onRuleProposed $ voteWith_ unanimity $ assessOnEveryVote |]

rVictory5Rules = SubmitRule "Victory 5 accepted rules"
                            "Victory is achieved if you have 5 active rules"
                            [cr|victoryXRules 5|]

rVoteMajority = SubmitRule "Majority Vote"
                            "A proposed rule will be activated if a majority of players is reached, with a minimum of 2 players, and within oone day"
                            [cr|onRuleProposed $ voteWith_ (majority `withQuorum` 2) $ assessOnEveryVote >> assessOnTimeDelay oneMinute |]


initialGame :: ServerHandle -> StateT LoggedGame IO ()
initialGame sh = mapM_ addR [rVoteUnanimity, rVictory5Rules]
   where addR r = execGameEvent' (Just $ getRuleFunc sh) (SystemAddRule r)

initialLoggedGame :: GameName -> GameDesc -> UTCTime -> ServerHandle -> IO LoggedGame
initialLoggedGame name desc date sh = do
   let lg = LoggedGame (emptyGame name desc date) []
   execStateT (initialGame sh) lg

displayMulti :: Multi -> String
displayMulti m = concatMap (displayGame . _game) (_games m)
