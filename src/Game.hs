{-# LANGUAGE StandaloneDeriving, GADTs, DeriveDataTypeable,
    FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies,
    TypeOperators, FlexibleInstances, NoMonomorphismRestriction,
    TypeSynonymInstances #-}

-- | This module implements Game management.
-- a game is a set of rules, and results of actions made by players (usually vote results)
-- the module manages the effects of rules over each others.
module Game (initialGame, activeRules, execWithGame, pendingRules, rejectedRules) where

import Language.Nomyx.Rule
import Control.Monad.State
import Data.List
import Language.Nomyx.Expression
import Language.Nomyx.Evaluation
import Language.Nomyx.Examples


-- | the initial rule set for a game.
rVoteUnanimity = Rule  {
    rNumber       = 1,
    rName         = "Unanimity Vote",
    rDescription  = "A proposed rule will be activated if all players vote for it",
    rProposedBy   = 0,
    rRuleCode     = "onRuleProposed $ voteWith unanimity",
    rRuleFunc     = onRuleProposed $ voteWith unanimity,
    rStatus       = Active,
    rAssessedBy   = Nothing}

rVictory5Rules = Rule  {
    rNumber       = 2,
    rName         = "Victory 5 accepted rules",
    rDescription  = "Victory is achieved if you have 5 active rules",
    rProposedBy   = 0,
    rRuleCode     = "victoryXRules 5",
    rRuleFunc     = victoryXRules 5,
    rStatus       = Active,
    rAssessedBy   = Nothing}

emptyGame name date = Game { gameName      = name,
                             gameDesc      = "",
                             rules         = [],
                             players       = [],
                             variables     = [],
                             events        = [],
                             outputs       = [],
                             victory       = [],
                             currentTime   = date}

--initialGame :: Game
initialGame name date = flip execState (emptyGame name date) $ do
    evAddRule rVoteUnanimity
    evActivateRule (rNumber rVoteUnanimity) 0
    evAddRule rVictory5Rules
    evActivateRule (rNumber rVictory5Rules) 0

-- | the initial rule set for a game.
rApplicationMetaRule = Rule  {
    rNumber       = 0,
    rName         = "Evaluate rule using meta-rules",
    rDescription  = "a proposed rule will be activated if all active metarules return true",
    rProposedBy   = 0,
    rRuleCode     = "applicationMetaRule",
    rRuleFunc     = onRuleProposed checkWithMetarules,
    rStatus       = Active,
    rAssessedBy   = Nothing}

-- | An helper function to use the state transformer GameState.
-- It additionally sets the current time.
execWithGame :: UTCTime -> State Game () -> Game -> Game
execWithGame t gs g = execState gs g {currentTime = t}


--accessors

activeRules :: Game -> [Rule]
activeRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Active) . rules

pendingRules :: Game -> [Rule]
pendingRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Pending) . rules

rejectedRules :: Game -> [Rule]
rejectedRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Reject) . rules




instance Ord PlayerInfo where
   h <= g = (playerNumber h) <= (playerNumber g)
