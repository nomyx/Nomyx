{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | This module containt the type definitions necessary to build a Nomic rule. 
module Language.Nomyx.Expression where

import Data.Typeable
import Data.Time
import Control.Applicative hiding (Const)
import Data.Lens.Template
import Control.Monad.Error

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int
type RuleName = String
type RuleDesc = String
type RuleText = String
type RuleCode = String
type EventNumber = Int
type EventName = String
type VarName = String
type Code = String
type OutputNumber = Int

-- * Nomyx Expression

data Eff = Effect | NoEffect
type Effect = 'Effect
type NoEffect = 'NoEffect

-- | A Nomex (Nomyx Expression) allows the players to write rules.
-- within the rules, you can access and modify the state of the game.
type Nomex = Exp Effect

-- | A NomexNE (Nomyx Expression No Effect) is a specialisation of the type that guaranties
-- that the instructions will have no effects.
type NomexNE = Exp NoEffect

data Exp :: Eff -> * -> *   where
   --Variables management
   NewVar         :: (Typeable a, Show a) => VarName -> a -> Nomex (Maybe (V a))
   ReadVar        :: (Typeable a, Show a) => V a -> Exp NoEffect (Maybe a)
   WriteVar       :: (Typeable a, Show a) => V a -> a -> Nomex Bool
   DelVar         :: (V a) -> Nomex Bool
   --Events management
   OnEvent        :: Typeable e => Event e -> ((EventNumber, EventData e) -> Nomex ()) -> Nomex EventNumber
   DelEvent       :: EventNumber -> Nomex Bool
   DelAllEvents   :: Typeable e => Event e -> Nomex ()
   SendMessage    :: (Typeable a, Show a) => Event (Message a) -> a -> Nomex ()
   --Rules management
   ProposeRule    :: RuleInfo -> Nomex Bool
   ActivateRule   :: RuleNumber -> Nomex Bool
   RejectRule     :: RuleNumber -> Nomex Bool
   AddRule        :: RuleInfo -> Nomex Bool
   ModifyRule     :: RuleNumber -> RuleInfo -> Nomex Bool
   GetRules       :: NomexNE [RuleInfo]
   --Players management
   GetPlayers     :: NomexNE [PlayerInfo]
   SetPlayerName  :: PlayerNumber -> PlayerName -> Nomex Bool
   DelPlayer      :: PlayerNumber -> Nomex Bool
   --Outputs
   NewOutput      :: Maybe PlayerNumber -> NomexNE String -> Nomex OutputNumber
   GetOutput      :: OutputNumber -> NomexNE (Maybe String)
   UpdateOutput   :: OutputNumber -> NomexNE String -> Nomex Bool
   DelOutput      :: OutputNumber -> Nomex Bool
   --Mileacenous
   SetVictory     :: NomexNE [PlayerNumber] -> Nomex ()
   CurrentTime    :: NomexNE UTCTime
   SelfRuleNumber :: NomexNE RuleNumber
   --Monadic bindings
   Return         :: a -> Exp e a
   Bind           :: Exp e a -> (a -> Exp e b) -> Exp e b
   ThrowError     :: String -> Exp Effect a
   CatchError     :: Nomex a -> (String -> Nomex a) -> Nomex a
   LiftEffect     :: NomexNE a -> Nomex a
   Simu           :: Nomex a -> NomexNE Bool -> NomexNE Bool

instance Typeable1 (Exp NoEffect) where
    typeOf1 _ = mkTyConApp (mkTyCon3 "main" "Language.Nomyx.Expression" "Exp NoEffect") []

instance Typeable1 (Exp Effect) where
    typeOf1 _ = mkTyConApp (mkTyCon3 "main" "Language.Nomyx.Expression" "Exp Effect") []

liftEffect :: NomexNE a -> Nomex a
liftEffect = LiftEffect  

instance Monad (Exp a) where
   return = Return
   (>>=) = Bind
   
instance Functor (Exp a) where
   fmap f e = Bind e $ Return . f

instance Applicative (Exp a) where
   pure = Return
   f <*> a = do
      f' <- f
      a' <- a
      return $ f' a'

instance MonadError String Nomex where
   throwError = ThrowError
   catchError = CatchError

instance Typeable a => Show (Exp NoEffect a) where
   show e = "<" ++ (show $ typeOf e) ++ ">"

instance Typeable a => Show (Exp Effect a) where
   show e = "<" ++ (show $ typeOf e) ++ ">"

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '<' : (show . typeOf) e ++ ">"

-- * Variables

-- | a container for a variable name and type
data V a = V {varName :: String} deriving Typeable

-- * Events

-- | events types
data Player = Arrive | Leave deriving (Typeable, Show, Eq)
data RuleEvent = Proposed | Activated | Rejected | Added | Modified | Deleted deriving (Typeable, Show, Eq)
data Time           deriving Typeable
data EvRule         deriving Typeable
data Message m      deriving Typeable
data Victory        deriving Typeable
data Input a = Input PlayerNumber String (InputForm a) deriving Typeable
data InputForm a = Radio [(a, String)]
                 | Text
                 | TextArea
                 | Button
                 | Checkbox [(a, String)]
                 deriving Typeable

-- | events names
data Event a where
    Player      :: Player ->                     Event Player
    RuleEv      :: RuleEvent ->                  Event RuleEvent
    Time        :: UTCTime ->                    Event Time
    Message     :: String ->                     Event (Message m)
    InputEv     :: (Typeable a, Show a, Eq a) => Input a -> Event (Input a)
    Victory     ::                               Event Victory
    deriving (Typeable)

-- data sent back by inputs
data InputData a = RadioData a
                 | CheckboxData [a]
                 | TextData String
                 | TextAreaData String
                 | ButtonData

                  
-- | data associated with each events
data EventData a where
    PlayerData  ::             {playerData :: PlayerInfo}    -> EventData Player
    RuleData    ::             {ruleData :: RuleInfo}        -> EventData RuleEvent
    TimeData    ::             {timeData :: UTCTime}         -> EventData Time
    MessageData :: (Show m) => {messageData :: m}            -> EventData (Message m)
    InputData   :: (Show a) => {inputData :: InputData a}    -> EventData (Input a)
    VictoryData ::             {victoryData :: VictoryCond}  -> EventData Victory
    deriving (Typeable)

deriving instance             Show      (Event a)
deriving instance (Show a) => Show      (InputForm a)
deriving instance (Show a) => Show      (Input a)
deriving instance             Show      (EventData a)
deriving instance (Show a) => Show      (InputData a)
deriving instance (Show a) => Show      (Message a)
deriving instance             Show      Victory
deriving instance             Show      Time
deriving instance             Eq        Time
deriving instance             Eq        Victory
deriving instance             Eq        EvRule
deriving instance             Eq        (Message m)
deriving instance             Eq        (Event e)
deriving instance (Eq e) =>   Eq        (Input e)
deriving instance (Eq e) =>   Eq        (InputForm e)


type Msg a = Event (Message a)
type MsgData a = EventData (Message a)

-- * Rule

-- | Type of a rule function.
type Rule = Nomex ()
  
-- | An informationnal structure about a rule
data RuleInfo = RuleInfo { _rNumber      :: RuleNumber,       -- number of the rule (must be unique) TO CHECK
                           _rName        :: RuleName,         -- short name of the rule 
                           _rDescription :: String,           -- description of the rule
                           _rProposedBy  :: PlayerNumber,     -- player proposing the rule
                           _rRuleCode    :: Code,             -- code of the rule as a string
                           _rRule        :: Rule,             -- function representing the rule (interpreted from rRuleCode)
                           _rStatus      :: RuleStatus,       -- status of the rule
                           _rAssessedBy  :: Maybe RuleNumber} -- which rule accepted or rejected this rule
                           deriving (Typeable, Show)

instance Eq RuleInfo where
    (RuleInfo {_rNumber=r1}) == (RuleInfo {_rNumber=r2}) = r1 == r2

instance Ord RuleInfo where
     (RuleInfo {_rNumber=r1}) <= (RuleInfo {_rNumber=r2}) = r1 <= r2

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

data VictoryCond = VictoryCond RuleNumber (NomexNE [PlayerNumber]) deriving (Show, Typeable)

partial :: String -> Nomex (Maybe a) -> Nomex a
partial s nm = do
   m <- nm
   case m of
      Just a -> return a
      Nothing -> throwError s

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

$( makeLenses [''RuleInfo, ''PlayerInfo] )

