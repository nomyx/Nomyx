
-- | Warning: Nomyx internals (not required to compose rules and play the game)
-- This module implements game engine.
-- the module manages the effects of rules over each others.
module Nomyx.Core.Engine(
   -- * Game management
   GameEvent(..),
   RuleEv(..),
   LoggedGame(..),
   GameName,
   Game(..),
   GameDesc(..),
   RuleEventInfo(..),
   execGameEvent, execGameEvent',
   execWithGame, execWithGame',
   game,
   emptyGame,
   getLoggedGame,
   gameName, gameDesc,
   players,
   getVictorious,

   -- * Variables management
   Var(..),

   -- * Rules management
   activeRules, pendingRules, rejectedRules,
   InterpretRule,

   -- * Events management
   events,
   getEventInfo,

   -- * Outputs management
   Output(..),
   Log(..),
   evalOutput,
   isOutput,

   -- * Time
   currentTime,

   -- * Misc
   tracePN,
   getChoiceEvents,
   getTextEvents,
   runEvaluate,
   getL
   ) where

import           Nomyx.Core.Engine.Evaluation
import           Nomyx.Core.Engine.GameEvents
import           Nomyx.Core.Engine.Test
import           Nomyx.Core.Engine.Types
import           Nomyx.Core.Engine.EvalUtils
import           Imprevu.Evaluation.Utils
