{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Evaluation of a Nomyx expression
module Event.Nomyx.Engine.Evaluation where

import           Control.Applicative
import           Control.Category            hiding (id)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Todo
import           Data.Typeable
import           Event.Nomyx.Expression
import           Event.Nomyx.Engine.EvalUtils
import           Event.Nomyx.Engine.EventEval
import           Event.Nomyx.Engine.Types     hiding (_vRuleNumber)
import           Event.Nomyx.Engine.Utils
import           Prelude                     hiding (log, (.))
import           System.Random



-- * Evaluation

-- | evaluate an effecful expression.
evalNomex :: Nomex a -> Evaluate a
evalNomex (OnEvent ev h)          = evOnEvent ev h
evalNomex (DelEvent en)           = evDelEvent en
evalNomex (SendMessage m d)       = evSendMessage m d

-- | evaluate an effectless expression.
evalNomexNE :: NomexNE a -> EvaluateNE a
evalNomexNE  GetEvents      = asks _events

evOnEvent :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> Nomex ()) -> Evaluate EventNumber
evOnEvent ev h = do
   evs <- use events
   let en = getFreeNumber (map _eventNumber evs)
   events %= (EventInfo en ev h SActive [] : )
   return en

evSendMessage :: (Typeable a, Show a) => Msg a -> a -> Evaluate ()
evSendMessage m = triggerEvent (Message m)

evDelEvent :: EventNumber -> Evaluate Bool
evDelEvent en = do
   evs <- use events
   case find ((== en) . getL eventNumber) evs of
      Nothing -> return False
      Just eh -> case _evStatus eh of
         SActive -> do
            events .= replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs
            return True
         SDeleted -> return False

evTriggerTime :: UTCTime -> Evaluate ()
evTriggerTime t = triggerEvent (Time t) t

--get the signals left to be completed in an event
getRemainingSignals :: EventInfo -> [EventInfo] -> [(SignalAddress, SomeSignal)]
getRemainingSignals (EventInfo _ e _ _ en) eis = case runEvaluateNE eis (getEventResult e en) of
   Done _ -> []
   Todo a -> a

runEvaluateNE :: [EventInfo] -> EvaluateNE a -> a
runEvaluateNE eis ev = runReader ev (EvalEnv eis evalNomex evalNomexNE)

runEvaluate :: [EventInfo] -> State EvalEnv a -> a
runEvaluate eis ev = evalState ev (EvalEnv eis evalNomex evalNomexNE)


displayEvent :: [EventInfo] -> EventInfo -> String
displayEvent eis ei@(EventInfo en _ _ s envi) =
   "event num: " ++ (show en) ++
   ", remaining signals: " ++ (show $ getRemainingSignals ei eis) ++ --TODO: display also event result?
   ", envs: " ++ (show envi) ++
   ", status: " ++ (show s)

