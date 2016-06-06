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

rAutoActivate :: RuleTemplate
rAutoActivate = RuleTemplate "AutoActivate"
                             "Any proposed rule will be automatically activated, without any vote"
                             [cr|return ()|]
                             "Kau"
                             Nothing
                             []
                             []

initialGame :: ServerHandle -> StateT GameInfo IO ()
initialGame sh = zoom loggedGame $ mapM_ addR [rAutoActivate]
   where addR rt = execGameEvent' (Just $ Left $ interRule sh) (ProposeRuleEv SystemAdd 0 rt)

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
defaultMulti s = Multi [] s (Library [rAutoActivate] [])

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
