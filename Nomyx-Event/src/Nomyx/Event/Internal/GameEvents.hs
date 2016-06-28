{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module implements the events that can affect a game.
module Nomyx.Core.Engine.GameEvents where

import Prelude hiding (log)
import Control.Monad.State
import Data.List
import Language.Nomyx.Expression
import Nomyx.Core.Engine.Evaluation
import Nomyx.Core.Engine.EventEval
import Nomyx.Core.Engine.Types
import Nomyx.Core.Engine.Utils
import Control.Lens
import Control.Category ((>>>))
import Control.Exception as E
import Data.Time
import Data.Maybe
import Data.Either


-- | the user has provided an input result
--inputResult :: PlayerNumber -> EventNumber -> SignalAddress -> FormField -> InputData -> State Game ()
--inputResult pn en sa ff ide = do
--   tracePN pn $ "input result: EventNumber " ++ show en ++ ", SignalAddress " ++ show sa ++ ", Form " ++ show ff ++ ", choice " ++ show ide
--   runSystemEval pn $ triggerInput ff ide sa en

getTimes :: EventInfo -> Game -> [UTCTime]
getTimes ei g = mapMaybe getTime (map snd $ getRemainingSignals ei g)

getTime :: SomeSignal -> Maybe UTCTime
getTime (SomeSignal (Time t)) = Just t
getTime _                    = Nothing

