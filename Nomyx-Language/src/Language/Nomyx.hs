{-# LANGUAGE Trustworthy #-}

-- | This module re-exports the elements necessary to compose a Nomyx rule.
module Language.Nomyx (module X)  where

import Language.Nomyx.Expression as X -- Nomyx Expression DSL
import Language.Nomyx.Outputs    as X -- create outputs
import Language.Nomyx.Inputs     as X -- create inputs
import Language.Nomyx.Events     as X -- create events
import Language.Nomyx.Players    as X -- manage players
import Language.Nomyx.Variables  as X -- create variables
import Language.Nomyx.Rules      as X -- manage rules
import Language.Nomyx.Vote       as X -- create votations
