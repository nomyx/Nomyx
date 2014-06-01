{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module implements game engine (for the nomyx language, see Language.Nomyx)
module Nomyx.Core.Engine.Game where

import Prelude hiding (log)
import Language.Nomyx.Expression
import Data.Lens.Template
import Data.Time
import Data.Typeable
import Data.Data

-- * Game

type GameName = String

-- | The state of the game:
data Game = Game { _gameName    :: GameName,
                   _gameDesc    :: GameDesc,
                   _rules       :: [RuleInfo],
                   _players     :: [PlayerInfo],
                   _variables   :: [Var],
                   _events      :: [EventHandler],
                   _outputs     :: [Output],
                   _victory     :: Maybe VictoryCond,
                   _logs        :: [Log],
                   _currentTime :: UTCTime}
                   deriving (Typeable)

data GameDesc = GameDesc { _desc :: String, _agora :: String} deriving (Eq, Show, Read, Ord)

instance Eq Game where
   (Game {_gameName=gn1}) == (Game {_gameName=gn2}) = gn1 == gn2

instance Ord Game where
   compare (Game {_gameName=gn1}) (Game {_gameName=gn2}) = compare gn1 gn2

emptyGame name desc date = Game {
   _gameName      = name,
   _gameDesc      = desc,
   _rules         = [],
   _players       = [],
   _variables     = [],
   _events        = [],
   _outputs       = [],
   _victory       = Nothing,
   _logs          = [],
   _currentTime   = date}


-- * Variables

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a) =>
   Var { _vRuleNumber :: RuleNumber,
         _vName       :: String,
         vData        :: a}

instance Show Var where
   show (Var a b c) = "Rule number = " ++ (show a) ++ ", Name = " ++ (show b) ++ ", Value = " ++ (show c) ++ "\n"

-- * Events

data EventHandler = forall e. (Typeable e, Show e) => EH
        {_eventNumber :: EventNumber,
         _ruleNumber  :: RuleNumber,
         event        :: Event e,
         handler      :: (EventNumber, e) -> Nomex (),
         _evStatus    :: Status,
         _env         :: [EventRes]}

data EventRes = forall e. (Typeable e, Show e) => EventRes
       {bev :: Field e,
        res :: e}

deriving instance Show EventRes

data Status = SActive | SDeleted deriving (Eq, Show)


instance Eq EventHandler where
    (EH {_eventNumber=e1}) == (EH {_eventNumber=e2}) = e1 == e2

instance Ord EventHandler where
    (EH {_eventNumber=e1}) <= (EH {_eventNumber=e2}) = e1 <= e2


-- * Outputs

data Output = Output { _outputNumber  :: OutputNumber,         -- number of the output
                       _oRuleNumber   :: RuleNumber,           -- rule that triggered the output
                       _oPlayerNumber :: (Maybe PlayerNumber), -- player to display the output to (Nothing means display to all players)
                       _output        :: NomexNE String,       -- output string
                       _oStatus       :: Status}               -- status of the output
                       deriving (Show)

-- * Logs

data Log = Log { _lPlayerNumber :: Maybe PlayerNumber,
                 _lTime         :: UTCTime,
                 _lMsg          :: String}
                 deriving (Show)

-- * Rules

data SubmitRule = SubmitRule RuleName RuleDesc RuleCode deriving (Show, Read, Eq, Ord, Data, Typeable)


$( makeLenses [''Game, ''GameDesc, ''EventHandler, ''Var, ''Output] )



