{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}

-- | This module contains the type definitions necessary to build a Nomic rule.
module Language.Nomyx.Types (
  module Language.Nomyx.Types,
  module Imprevu.Types)
  where

import           Control.Lens
import           Control.Monad.Except
import           Data.Data           (Data)
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           System.Random
import           Imprevu.Types
import           Imprevu

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int
type RuleName = String
type RuleDesc = String
type RuleText = String
type RuleCode = String
type OutputNumber = Int
type InputNumber = Int

-- * Nomyx.Types

data Nomex a  where
   --Variables management
   NewVar          :: (Typeable a, Show a) => VarName -> a -> Nomex (Maybe (V a))
   ReadVar         :: (Typeable a, Show a) => V a -> Nomex (Maybe a)
   WriteVar        :: (Typeable a, Show a) => V a -> a -> Nomex Bool
   DelVar          :: (V a) -> Nomex Bool
   --Events management
   OnEvent         :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> Nomex EventNumber
   DelEvent        :: EventNumber -> Nomex Bool
   GetEvents       :: Nomex [EventInfo]
   SendMessage     :: (Typeable a, Show a) => Msg a -> a -> Nomex ()
   --Rules management
   ProposeRule     :: RuleInfo -> Nomex Bool
   ActivateRule    :: RuleNumber -> Nomex Bool
   RejectRule      :: RuleNumber -> Nomex Bool
   AddRule         :: RuleInfo -> Nomex Bool
   ModifyRule      :: RuleNumber -> RuleInfo -> Nomex Bool
   GetRules        :: Nomex [RuleInfo]
   SelfRuleNumber  :: Nomex RuleNumber
   --Players management
   GetPlayers      :: Nomex [PlayerInfo]
   SetPlayerName   :: PlayerNumber -> PlayerName -> Nomex Bool
   DelPlayer       :: PlayerNumber -> Nomex Bool
   --Outputs
   NewOutput       :: Maybe PlayerNumber -> Nomex String -> Nomex OutputNumber
   GetOutput       :: OutputNumber -> Nomex (Maybe String)
   UpdateOutput    :: OutputNumber -> Nomex String -> Nomex Bool
   DelOutput       :: OutputNumber -> Nomex Bool
   --Victory
   SetVictory      :: Nomex [PlayerNumber] -> Nomex ()
   --Mileacenous
   GetCurrentTime  :: Nomex UTCTime
   GetRandomNumber :: Random a => (a, a) -> Nomex a
   --Monadic bindings
   Return          :: a -> Nomex a
   Bind            :: Nomex a -> (a -> Nomex b) -> Nomex b
   ThrowError      :: String -> Nomex a
   CatchError      :: Nomex a -> (String -> Nomex a) -> Nomex a
   Simu            :: Nomex a -> Nomex Bool -> Nomex Bool

deriving instance Typeable Nomex

instance Typeable a => Show (Nomex a) where
   show _ = "<Nomex a>"

instance Monad Nomex where
   return = Return
   (>>=) = Bind

instance Functor Nomex where
   fmap f e = Bind e $ Return . f

instance Applicative Nomex where
   pure = Return
   f <*> a = do
      f' <- f
      a' <- a
      return $ f' a'

instance MonadError String Nomex where
   throwError = ThrowError
   catchError = CatchError

instance EvMgt Nomex where
   onEvent         = OnEvent
   delEvent        = DelEvent
   getEvents       = GetEvents
   sendMessage     = SendMessage

instance SysMgt Nomex where
   getCurrentTime  = GetCurrentTime
   getRandomNumber = GetRandomNumber

instance VarMgt Nomex where
   newVar       = NewVar
   readVar      = ReadVar
   writeVar     = WriteVar
   delVar       = DelVar


-- * Events

type Event a = EventM Nomex a
type EventInfo = EventInfoN Nomex

-- | Events parameters
data Player    = Arrive | Leave deriving (Typeable, Show, Eq)
data RuleEvent = Proposed | Activated | Rejected | Added | Modified | Deleted deriving (Typeable, Show, Eq)


-- * Rule

-- | Type of a rule function.
type Rule = Nomex ()

-- | An informationnal structure about a rule
data RuleInfo = RuleInfo { _rNumber       :: RuleNumber,       -- number of the rule (must be unique)
                           _rProposedBy   :: PlayerNumber,     -- player proposing the rule
                           _rRule         :: Rule,             -- function representing the rule (interpreted from rRuleCode)
                           _rStatus       :: RuleStatus,       -- status of the rule
                           _rAssessedBy   :: Maybe RuleNumber, -- which rule accepted or rejected this rule
                           _rModules      :: [Module],         -- list of modules containing definition (in plain text)
                           _rRuleTemplate :: RuleTemplate}
                           deriving (Typeable, Show)


data RuleTemplate = RuleTemplate { _rName         :: RuleName,         -- short name of the rule
                                   _rDescription  :: String,           -- description of the rule
                                   _rRuleCode     :: RuleCode,         -- code of the rule as a string
                                   _rAuthor       :: String,           -- the name of the original author
                                   _rPicture      :: Maybe FilePath,   -- a file name for the illustration image
                                   _rCategory     :: [String],         -- categories
                                   _rDeclarations :: [FilePath]}       -- additional declarations (Haskell modules)
                                   deriving (Typeable, Show, Read, Data, Generic)

type Module = String

data ModuleInfo = ModuleInfo {_modPath :: FilePath,  -- file name of the module
                              _modContent :: Module} -- content of the module (or Nothing if module is present in library)
                              deriving (Eq, Read, Show, Typeable, Data, Generic, Ord)

instance Eq RuleInfo where
    (RuleInfo {_rNumber=r1}) == (RuleInfo {_rNumber=r2}) = r1 == r2

instance Ord RuleInfo where
    (RuleInfo {_rNumber=r1}) <= (RuleInfo {_rNumber=r2}) = r1 <= r2

instance Eq RuleTemplate where
    (RuleTemplate {_rName=r1}) == (RuleTemplate {_rName=r2}) = r1 == r2

instance Ord RuleTemplate where
    (RuleTemplate {_rName=r1}) <= (RuleTemplate {_rName=r2}) = r1 <= r2

-- | the status of a rule.
data RuleStatus = Active      -- Active rules forms the current Constitution
                | Pending     -- Proposed rules
                | Reject      -- Rejected rules
                deriving (Eq, Show, Typeable)

-- * Player

-- | informations on players
data PlayerInfo = PlayerInfo { _playerNumber :: PlayerNumber,
                               _playerName   :: String,
                               _playAs       :: Maybe PlayerNumber}
                               deriving (Eq, Typeable, Show)

instance Ord PlayerInfo where
   h <= g = (_playerNumber h) <= (_playerNumber g)

-- * Victory

data VictoryInfo = VictoryInfo { _vRuleNumber :: RuleNumber,
                                 _vCond       :: Nomex [PlayerNumber]}
                                 deriving (Show, Typeable)

-- * Miscellaneous

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '<' : (show . typeOf) e ++ ">"

enumAll :: (Enum a, Show a, Bounded a) => [(a, String)]
enumAll = map (\a -> (a, show a)) (enumFrom minBound)


makeLenses ''RuleInfo
makeLenses ''RuleTemplate
makeLenses ''PlayerInfo
makeLenses ''ModuleInfo
