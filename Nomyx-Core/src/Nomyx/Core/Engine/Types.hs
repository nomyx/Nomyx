{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Types for the engine
module Nomyx.Core.Engine.Types where

import Prelude hiding (log)
import Language.Nomyx.Expression
import Control.Lens hiding ((.=))
import Control.Applicative
import Data.Time
import Data.Typeable
import Data.Data
import Control.Monad.Error (ErrorT(..))
import Control.Monad.State
import Control.Monad.Reader
import GHC.Generics
import System.Random
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson
import Data.Time.Clock.POSIX
import Imprevu.Evaluation.EventEval hiding (events)
import Imprevu.Evaluation.InputEval

-- * Evaluation

-- | Environment necessary for the evaluation of any nomyx expressions or events
--TODO: should the first field be a "Maybe RuleNumber"?
--Indeed an evaluation is not always performed by a rule but also by the system (in which case we currently use rule number 0)
--data EvalEnv = EvalEnv { _eRuleNumber :: RuleNumber,
--                         _eGame :: Game,
--                         evalNomexFunc :: forall a. Nomex a -> Evaluate a}       -- evaluation function
--                        -- evalNomexNEFunc :: forall b. NomexNE b -> EvaluateNE b} -- evaluation function without effect

data EvalState = EvalState { _eGame :: Game,             -- game to be read/modified
                             _eRuleNumber :: RuleNumber} -- number of the rule requesting the evaluation

type EvalEnv = EvalEnvN Nomex EvalState

-- | Environment necessary for the evaluation of Nomex
type Evaluate a = EvaluateN Nomex EvalState a

-- | Environment necessary for the evaluation of NomexNE
--type EvaluateNE a = Reader EvalEnv a

-- | extract a rule from a code string and an environment
type InterpretRule = RuleCode -> [ModuleInfo] -> IO Rule

--class HasEvents n s where
--   getEvents :: s -> [EventInfoN n]
--   setEvents :: [EventInfoN n] -> s -> s

-- * Game

type GameName = String


-- | The state of the game:
data Game = Game { _gameName    :: GameName,
                   _gameDesc    :: GameDesc,
                   _rules       :: [RuleInfo],
                   _players     :: [PlayerInfo],
                   _variables   :: [Var],
                   _events      :: [RuleEventInfo],
                   _outputs     :: [Output],
                   _victory     :: Maybe VictoryInfo,
                   _logs        :: [Log],
                   _currentTime :: UTCTime,
                   _randomGen   :: StdGen}
                   deriving (Typeable)

data GameDesc = GameDesc { _desc :: String, _forumURL :: String} deriving (Eq, Show, Read, Ord)

instance Eq Game where
   (Game {_gameName=gn1}) == (Game {_gameName=gn2}) = gn1 == gn2

instance Ord Game where
   compare (Game {_gameName=gn1}) (Game {_gameName=gn2}) = compare gn1 gn2

emptyGame :: GameName -> GameDesc -> UTCTime -> StdGen -> Game
emptyGame name desc date gen = Game {
   _gameName      = name,
   _gameDesc      = desc,
   _rules         = [],
   _players       = [],
   _variables     = [],
   _events        = [],
   _outputs       = [],
   _victory       = Nothing,
   _logs          = [],
   _randomGen     = gen,
   _currentTime   = date}


-- | a list of possible events affecting a game
data GameEvent = JoinGame          PlayerNumber PlayerName
               | LeaveGame         PlayerNumber
               | ProposeRuleEv     RuleEv PlayerNumber RuleTemplate [ModuleInfo]
               | InputResult       PlayerNumber EventNumber SignalAddress InputView InputDataView
               | GLog              (Maybe PlayerNumber) String
               | TimeEvent         UTCTime
                deriving (Show, Read, Eq, Ord)

data RuleEv = Propose | Check | SystemAdd
                deriving (Show, Read, Eq, Ord)

data TimedEvent = TimedEvent UTCTime GameEvent deriving (Show, Read, Eq, Ord)

-- | A game being non serializable, we have to store events in parralel in order to rebuild the state latter.
data LoggedGame = LoggedGame { _game :: Game,
                               _gameLog :: [TimedEvent]}

instance Eq LoggedGame where
  (LoggedGame {_game=g1}) == (LoggedGame {_game=g2}) = g1 == g2

instance Ord LoggedGame where
  compare (LoggedGame {_game=g1}) (LoggedGame {_game=g2}) = compare g1 g2


-- * Variables

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a) =>
   Var { _vRuleNumber :: RuleNumber,
         _vName       :: String,
         vData        :: a}

vRuleNumber :: Lens' Var RuleNumber
vRuleNumber f (Var vr vn vd) = fmap (\vr' -> (Var vr' vn vd)) (f vr)

vName :: Lens' Var String
vName f (Var vr vn vd) = fmap (\vn' -> (Var vr vn' vd)) (f vn)

instance Show Var where
   show (Var a b c) = "Rule number = " ++ (show a) ++ ", Name = " ++ (show b) ++ ", Value = " ++ (show c) ++ "\n"



-- * Events

data RuleEventInfo = RuleEventInfo { _erRuleNumber :: RuleNumber,  -- the rule that created the event
                                     _erEventInfo :: EventInfo}    -- event informations
                                     deriving (Eq)

instance Ord RuleEventInfo where
     (RuleEventInfo rn1 _) <= (RuleEventInfo rn2 _) = rn1 <= rn2

-- a form field
--data FormField = RadioField    PlayerNumber String [(Int, String)]
--               | TextField     PlayerNumber String
--               | TextAreaField PlayerNumber String
--               | ButtonField   PlayerNumber String
--               | CheckboxField PlayerNumber String [(Int, String)]
--                 deriving (Show, Read, Ord, Eq, Generic)
--
---- data sent back by the form fields
--data InputData = RadioData    Int
--               | CheckboxData [Int]
--               | TextData     String
--               | TextAreaData String
--               | ButtonData
--                 deriving (Show, Read, Eq, Ord)
--

-- * Outputs

data Output = Output { _outputNumber  :: OutputNumber,         -- number of the output
                       _oRuleNumber   :: RuleNumber,           -- rule that triggered the output
                       _oPlayerNumber :: (Maybe PlayerNumber), -- player to display the output to (Nothing means display to all players)
                       _output        :: Nomex String,         -- output string
                       _oStatus       :: Status}               -- status of the output
                       deriving (Show)

instance Eq Output where
   (Output {_outputNumber=on1}) == (Output {_outputNumber=on2}) = on1 == on2

instance Ord Output where
     (Output {_outputNumber=r1}) <= (Output {_outputNumber=r2}) = r1 <= r2

-- * Logs

data Log = Log { _lPlayerNumber :: Maybe PlayerNumber,
                 _lTime         :: UTCTime,
                 _lMsg          :: String}
                 deriving (Show)

makeLenses ''Game
makeLenses ''GameDesc
makeLenses ''Var
makeLenses ''Output
makeLenses ''EvalState
makeLenses ''LoggedGame
makeLenses ''RuleEventInfo

time0 = posixSecondsToUTCTime 0

instance ToJSON Game where
   toJSON (Game name desc _ _ _ _ _ _ _ _ _) =
      object ["gameName" .= name,
              "gameDesc" .= desc]

instance FromJSON Game where
   parseJSON (Object v) = Game <$>
      v .: "gameName" <*>
      v .: "gameDesc" <*>
      pure [] <*>
      pure [] <*>
      pure [] <*>
      pure [] <*>
      pure [] <*>
      pure Nothing <*>
      pure [] <*>
      pure time0  <*>
      pure (mkStdGen 0)
   parseJSON _ = mzero

instance ToJSON RuleTemplate where
   toJSON (RuleTemplate n d rc a p cs ds) =
      object ["name" .= n,
              "desc" .= d,
              "rule" .= rc,
              "author" .= a,
              "picture" .= p,
              "category" .= cs,
              "decls" .= ds]

instance FromJSON RuleTemplate where
   parseJSON (Object v) = RuleTemplate <$>
      v .: "name" <*>
      v .: "desc" <*>
      v .: "rule" <*>
      v .: "author" <*>
      v .: "picture" <*>
      v .: "category" <*>
      v .: "decls"
   parseJSON _ = mzero

instance HasEvents Nomex EvalState where
   getEvents s = map _erEventInfo (_events $ _eGame s)
   setEvents eis s = (eGame . events) .~ (getreis (_events $ _eGame s) eis) $ s where
     getreis reis eis = if (length reis /= length eis) then error "setEvents" else zipWith (\rei ei -> rei {_erEventInfo = ei}) reis eis

--data RuleEventInfo = RuleEventInfo { _erRuleNumber :: RuleNumber,  -- the rule that created the event
--                                     _erEventInfo :: EventInfo}    -- event informations

$(deriveJSON defaultOptions ''TimedEvent)
$(deriveJSON defaultOptions ''GameEvent)
$(deriveJSON defaultOptions ''InputView)
$(deriveJSON defaultOptions ''InputDataView)
$(deriveJSON defaultOptions ''RuleEv)
-- $(deriveJSON defaultOptions ''FormField)
-- $(deriveJSON defaultOptions ''InputData)
$(deriveJSON defaultOptions ''GameDesc)
$(deriveJSON defaultOptions ''StdGen)
$(deriveJSON defaultOptions ''SignalAddressElem)
$(deriveJSON defaultOptions ''LoggedGame)
$(deriveJSON defaultOptions ''ModuleInfo)
