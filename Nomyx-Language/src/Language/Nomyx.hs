{-# LANGUAGE Trustworthy #-}

-- | This module re-exports the elements necessary to compose a Nomyx rule.
module Language.Nomyx (
   module Language.Nomyx.Expression,
   module Language.Nomyx.Outputs,
   module Language.Nomyx.Inputs,
   module Language.Nomyx.Events,
   module Language.Nomyx.Players,
   module Language.Nomyx.Variables,
   module Language.Nomyx.Rules,
   module Language.Nomyx.Vote)  where

import Language.Nomyx.Expression -- Nomyx Expression DSL
import Language.Nomyx.Outputs    -- create outputs
import Language.Nomyx.Inputs     -- create inputs
import Language.Nomyx.Events     -- create events
import Language.Nomyx.Players    -- manage players
import Language.Nomyx.Variables  -- create variables
import Language.Nomyx.Rules      -- manage rules
import Language.Nomyx.Vote       -- create votations
