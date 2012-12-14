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
import Examples
import Data.Time.Clock as T


-- | the initial rule set for a game.
rApplicationMetaRule = Rule  {
    rNumber       = 1,
    rName         = "Evaluate rule using meta-rules",
    rDescription  = "a proposed rule will be activated if all active metarules return true",
    rProposedBy   = 0,
    rRuleCode     = "applicationMetaRule",
    rRuleFunc     = applicationMetaRule,
    rStatus       = Active,
    rAssessedBy   = Nothing}

rVoteUnanimity = Rule  {
    rNumber       = 2,
    rName         = "Vote Unanimity",
    rDescription  = "meta-rule: return true if all players vote positively for a rule",
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


-- | An helper function to use the state transformer GameState.
execWithGame :: StateT Game IO () -> Game -> IO Game
execWithGame gs g = do
    t <- T.getCurrentTime
    let g' = g {currentTime = t}
    execStateT gs g'


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
