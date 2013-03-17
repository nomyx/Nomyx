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
import Data.Lens

-- | the initial rule set for a game.
rVoteUnanimity = Rule  {
    _rNumber       = 1,
    _rName         = "Unanimity Vote",
    _rDescription  = "A proposed rule will be activated if all players vote for it",
    _rProposedBy   = 0,
    _rRuleCode     = "onRuleProposed $ voteWith unanimity",
    _rRuleFunc     = onRuleProposed $ voteWith unanimity $ assessOnEveryVotes,
    _rStatus       = Active,
    _rAssessedBy   = Nothing}

rVictory5Rules = Rule  {
    _rNumber       = 2,
    _rName         = "Victory 5 accepted rules",
    _rDescription  = "Victory is achieved if you have 5 active rules",
    _rProposedBy   = 0,
    _rRuleCode     = "victoryXRules 5",
    _rRuleFunc     = victoryXRules 5,
    _rStatus       = Active,
    _rAssessedBy   = Nothing}

emptyGame name desc date = Game { _gameName      = name,
                                  _gameDesc      = desc,
                                  _rules         = [],
                                  _players       = [],
                                  _variables     = [],
                                  _events        = [],
                                  _outputs       = [],
                                  _victory       = [],
                                  _currentTime   = date}

initialGame :: GameName -> GameDesc -> UTCTime -> Game
initialGame name desc date = flip execState (emptyGame name desc date) $ do
    evAddRule rVoteUnanimity
    evActivateRule (_rNumber rVoteUnanimity) 0
    evAddRule rVictory5Rules
    evActivateRule (_rNumber rVictory5Rules) 0

-- | the initial rule set for a game.
rApplicationMetaRule = Rule  {
    _rNumber       = 0,
    _rName         = "Evaluate rule using meta-rules",
    _rDescription  = "a proposed rule will be activated if all active metarules return true",
    _rProposedBy   = 0,
    _rRuleCode     = "applicationMetaRule",
    _rRuleFunc     = onRuleProposed checkWithMetarules,
    _rStatus       = Active,
    _rAssessedBy   = Nothing}

-- | An helper function to use the state transformer GameState.
-- It additionally sets the current time.
execWithGame :: UTCTime -> State Game () -> Game -> Game
execWithGame t gs g = execState gs g{_currentTime = t}


--accessors

activeRules :: Game -> [Rule]
activeRules = sort . filter ((==Active) . getL rStatus) . _rules

pendingRules :: Game -> [Rule]
pendingRules = sort . filter ((==Pending) . getL rStatus) . _rules

rejectedRules :: Game -> [Rule]
rejectedRules = sort . filter ((==Reject) . getL rStatus) . _rules

instance Ord PlayerInfo where
   h <= g = (_playerNumber h) <= (_playerNumber g)
