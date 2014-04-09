
-- | Warning: Nomyx internals (not required to compose rules and play the game)
-- This module implements game engine.
-- the module manages the effects of rules over each others.
-- This module is not required
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
   gameName,
   players,
   getVictorious,

   -- * Variables management
   Var(..),

   -- * Rules management
   SubmitRule(..),
   activeRules, pendingRules, rejectedRules,

   -- * Events management
   EventHandler(..),
   Status(..),
   getEventHandler,
   events,
   getChoiceEvents,
   getTextEvents,

   -- * Inputs management
   UInputData(..),

   -- * Outputs management
   Output(..),
   Log(..),
   evalOutput,
   isOutput,

   -- * Time
   getTimes,
   currentTime,

   -- * Misc
   tracePN,
   replaceWith
   ) where

import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.Game
import Nomyx.Core.Engine.GameEvents
import Nomyx.Core.Engine.Utils

