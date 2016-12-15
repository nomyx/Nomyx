{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module manages multi-player games.
module Nomyx.Core.Multi where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State
import           Data.List
import           Data.Time                           as T
import           Nomyx.Language
import           Nomyx.Core.Engine                   as G
import           Nomyx.Core.Engine.Interpret
import           Nomyx.Core.Quotes                   (cr)
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           System.Random


rAutoActivate :: RuleTemplate
rAutoActivate = RuleTemplate "AutoActivate"
                             "Any proposed rule will be automatically activated, without any vote"
                             [cr|autoActivate|]
                             "Kau"
                             Nothing
                             []
                             []

initialGame :: StateT GameInfo IO ()
initialGame = zoom loggedGame $ mapM_ addR [rAutoActivate]
   where addR rt = execGameEvent' Nothing (ProposeRuleEv SystemAdd 0 rt [])

initialGameInfo :: GameName -> GameDesc -> Bool -> Maybe PlayerNumber -> UTCTime -> IO GameInfo
initialGameInfo name desc isPub mpn date = do
   let gen = mkStdGen 0
   let lg = GameInfo { _loggedGame = LoggedGame (emptyGame name desc date gen) [],
                       _ownedBy    = mpn,
                       _forkedFromGame = Nothing,
                       _isPublic = isPub,
                       _startedAt  = date}
   execStateT initialGame lg

defaultMulti :: Settings -> Library -> Multi
defaultMulti s lib = Multi [] s lib

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
