{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Control.Monad.Reader
import GHC.Generics

-- * Evaluation

--TODO: should the first field be a "Maybe RuleNumber"?
--Indeed an evaluation is not always performed by a rule but also by the system (in which case we currently use rule number 0)
data EvalEnv = EvalEnv { _eRuleNumber :: RuleNumber,  -- number of the rule requesting the evaluation
                         _eGame :: Game}              -- game to be read/modified

type Evaluate   a = ErrorT String (State EvalEnv) a
type EvaluateNE a = Reader EvalEnv a

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
                   _victory     :: Maybe VictoryInfo,
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

-- a form field
data FormField = RadioField PlayerNumber String [(Int, String)]
               | TextField PlayerNumber String
               | TextAreaField PlayerNumber String
               | ButtonField PlayerNumber String
               | CheckboxField PlayerNumber String [(Int, String)]
                 deriving (Show, Read, Ord, Eq, Generic)

-- data sent back by the form fields
data InputData = RadioData Int
               | CheckboxData [Int]
               | TextData String
               | TextAreaData String
               | ButtonData
                 deriving (Show, Read, Eq, Ord)


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


$( makeLenses [''Game, ''GameDesc, ''Var, ''Output, ''EvalEnv] )





