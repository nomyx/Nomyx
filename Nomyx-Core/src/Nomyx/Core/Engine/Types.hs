{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Types for the engine
module Nomyx.Core.Engine.Types where

import Prelude hiding (log)
import Language.Nomyx.Expression
import Data.Lens.Template
import Data.Time
import Data.Typeable
import Data.Data
import Control.Monad.Error (ErrorT(..))
import Control.Monad.State

-- * Game

type GameName = String

-- | The state of the game:
data Game = Game { _gameName    :: GameName,
                   _gameDesc    :: GameDesc,
                   _rules       :: [RuleInfo],
                   _players     :: [PlayerInfo],
                   _variables   :: [Var],
                   _events      :: [EventInfo],
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

type Evaluate a = ErrorT String (State Game) a

-- data sent back by the forms
data InputData = RadioData Int
               | CheckboxData [Int]
               | TextData String
               | TextAreaData String
               | ButtonData
                 deriving (Show, Read, Eq, Ord)

data SomeData = forall e. (Typeable e, Show e) => SomeData e
deriving instance Show SomeData

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


$( makeLenses [''Game, ''GameDesc, ''Var, ''Output] )





