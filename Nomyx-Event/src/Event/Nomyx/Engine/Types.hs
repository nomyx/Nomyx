{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the engine
module Event.Nomyx.Engine.Types where

import Prelude hiding (log)
import Event.Nomyx.Expression
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

-- * Evaluation

-- | Environment necessary for the evaluation of any nomyx expressions or events
--TODO: should the first field be a "Maybe RuleNumber"?
--Indeed an evaluation is not always performed by a rule but also by the system (in which case we currently use rule number 0)
data EvalEnv = EvalEnv { _events      :: [EventInfo],
                         evalNomexFunc :: forall a. Nomex a -> Evaluate a}       -- evaluation function
                        -- evalNomexNEFunc :: forall b. NomexNE b -> EvaluateNE b} -- evaluation function without effect

-- | Environment necessary for the evaluation of Nomex
type Evaluate   a = ErrorT String (State EvalEnv ) a

-- | Environment necessary for the evaluation of NomexNE
type EvaluateNE a = Reader EvalEnv a


-- * Events

-- a form field
data FormField = RadioField    String [(Int, String)]
               | TextField     String
               | TextAreaField String
               | ButtonField   String
               | CheckboxField String [(Int, String)]
                 deriving (Show, Read, Ord, Eq, Generic)

-- data sent back by the form fields
data InputData = RadioData    Int
               | CheckboxData [Int]
               | TextData     String
               | TextAreaData String
               | ButtonData
                 deriving (Show, Read, Eq, Ord)


makeLenses ''EvalEnv


$(deriveJSON defaultOptions ''FormField)
$(deriveJSON defaultOptions ''InputData)
$(deriveJSON defaultOptions ''SignalAddressElem)
