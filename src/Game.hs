{-# LANGUAGE StandaloneDeriving, GADTs, DeriveDataTypeable,
    FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies,
    TypeOperators, FlexibleInstances, NoMonomorphismRestriction,
    TypeSynonymInstances #-}

-- | This module implements Game management.
-- a game is a set of rules, and results of actions made by players (usually vote results)
-- the module manages the effects of rules over each others.
module Game (GameState, GameStateWith, initialGame, activeRules, runWithGame, pendingRules, rejectedRules) where

import Language.Nomyx.Rule
import Control.Monad.State
import Data.List
import Language.Nomyx.Expression
import Language.Nomyx.Evaluation
import Examples

-- | the initial rule set for a game.
rApplicationMetaRule = Rule  {
    rNumber       = 1,
    rName         = "Evaluate meta-rules",
    rDescription  = "all active metarules will be automatically used to evaluate a proposed rule",
    rProposedBy   = 0,
    rRuleCode     = "applicationMetaRule",
    rRuleFunc     = applicationMetaRule,
    rStatus       = Active,
    rAssessedBy   = Nothing}

rVoteUnanimity = Rule  {
    rNumber       = 2,
    rName         = "Vote Unanimity",
    rDescription  = "meta-rule: a new rule will be accepted if all players vote positively",
    rProposedBy   = 0,
    rRuleCode     = "vote unanimity",
    rRuleFunc     = vote unanimity,
    rStatus       = Active,
    rAssessedBy   = Nothing}

rVictory5Rules = Rule  {
    rNumber       = 3,
    rName         = "Victory 5 accepted rules",
    rDescription  = "Victory is achieved if you have 5 active rules",
    rProposedBy   = 0,
    rRuleCode     = "victoryXRules 5",
    rRuleFunc     = victoryXRules 5,
    rStatus       = Active,
    rAssessedBy   = Nothing}

emptyGame name date = Game { gameName      = name,
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
    evAddRule rApplicationMetaRule
    evActivateRule (rNumber rApplicationMetaRule) 0
    evAddRule rVictory5Rules
    evActivateRule (rNumber rVictory5Rules) 0


-- | Allow to pass around the state of the game while making IO on a specified Handle:
type GameState = StateT Game IO ()

type GameStateWith a = StateT Game IO a

-- | An helper function that makes it very clear how to use the state transformer GameState.
runWithGame :: Game -> GameState -> IO Game
runWithGame = flip execStateT


--accessors

activeRules :: Game -> [Rule]
activeRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Active) . rules

pendingRules :: Game -> [Rule]
pendingRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Pending) . rules

rejectedRules :: Game -> [Rule]
rejectedRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Reject) . rules

instance Eq Game where
   (Game name1 _ _ _ _ _ _ _) == (Game name2 _ _ _ _ _ _ _) = name1 == name2

instance Ord Game where
   compare (Game name1 _ _ _ _ _ _ _) (Game name2 _ _ _ _ _ _ _) = compare name1 name2


instance Ord PlayerInfo where
   h <= g = (playerNumber h) <= (playerNumber g)
