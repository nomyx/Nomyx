{-# LANGUAGE Trustworthy #-}

-- | This module re-exports the elements necessary to compose a Nomyx rule.
module Nomyx.Language (
   module Nomyx.Language.Outputs,
   module Nomyx.Language.Inputs,
   module Nomyx.Language.Events,
   module Nomyx.Language.Messages,
   module Nomyx.Language.Players,
   module Nomyx.Language.Variables,
   module Nomyx.Language.Rules,
   module Nomyx.Language.Types)  where

import Nomyx.Language.Outputs    -- create outputs
import Nomyx.Language.Inputs     -- create inputs
import Nomyx.Language.Events     -- create events
import Nomyx.Language.Messages   -- inter-rule communication
import Nomyx.Language.Players    -- manage players
import Nomyx.Language.Variables  -- create variables
import Nomyx.Language.Rules      -- manage rules
import Nomyx.Language.Types      -- Nomyx Expression DSL
