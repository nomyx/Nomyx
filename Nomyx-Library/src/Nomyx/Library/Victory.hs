{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | This file gives a list of example rules that the players can submit.
--You can copy-paste them in the field "Code" of the web GUI.
--You can copy either the name of the function (i.e. "helloWorld") or its body (i.e. "outputAll_ "hello, world!""), but NOT both.
--Don't hesitate to get inspiration from there and create your own rules!
module Nomyx.Library.Victory where

import Data.Function
import Data.List
import Control.Arrow
import Control.Monad
import Nomyx.Language
import Nomyx.Library.Bank


-- | set the victory for players having more than X accepted rules
victoryXRules :: Int -> Rule
victoryXRules x = setVictory $ do
    rs <- getRules
    let counts :: [(PlayerNumber,Int)]
        counts = map (_rProposedBy . head &&& length) $ groupBy ((==) `on` _rProposedBy) rs
    let victorious = map fst $ filter ((>= x) . snd) counts
    return victorious

victoryXEcu :: Int -> Rule
victoryXEcu x = setVictory $ do
    as <- readVar accounts
    let victorious as = map fst $ filter ((>= x) . snd) as
    return $ maybe [] victorious as

-- | Only one player can achieve victory: No group victory.
-- Forbidding group victory usually becomes necessary when lowering the voting quorum:
-- a coalition of players could simply force a "victory" rule and win the game.
noGroupVictory ::  Rule
noGroupVictory = do
   let testVictory (VictoryInfo _ cond) = do
       vics <- cond
       when (length vics >1) $ setVictory (return []) --unset victory condition
   void $ onEvent_ victoryEvent testVictory

-- | Rule that state that you win. Good luck on having this accepted by other players ;)
iWin :: Rule
iWin = getProposerNumber >>= giveVictory


