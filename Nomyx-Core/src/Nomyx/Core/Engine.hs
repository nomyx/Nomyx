
-- | Warning: Nomyx internals (not required to compose rules and play the game)
-- This module implements game engine.
-- the module manages the effects of rules over each others.
module Nomyx.Core.Engine(
   -- * Game management
   GameEvent(..),
   LoggedGame(..),
   GameName,
   Game(..),
   GameDesc(..),
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
   SubmitRule(..),
   activeRules, pendingRules, rejectedRules,

   -- * Events management
   events,
   getRemainingSignals,
   getEventInfo,
   getFormField,

   -- * Inputs management
   FormField(..),
   InputData(..),

   -- * Outputs management
   Output(..),
   Log(..),
   evalOutput,
   isOutput,

   -- * Time
   getTimes,
   getGameTimes,
   currentTime,

   -- * Misc
   tracePN,
   replaceWith,
   getChoiceEvents,
   getTextEvents,
   runEvaluate,
   getL
   ) where

import           Nomyx.Core.Engine.Evaluation
import           Nomyx.Core.Engine.EventEval
import           Nomyx.Core.Engine.GameEvents
import           Nomyx.Core.Engine.Test
import           Nomyx.Core.Engine.Types
import           Nomyx.Core.Engine.Utils
