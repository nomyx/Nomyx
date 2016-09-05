
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Imprevu.Time where

import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.Event
import Data.Time
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Typeable
import Control.Lens



type Time = Signal UTCTime UTCTime
type EvalFunc n s = (s -> EvalEnv n s)

launchTimeEvents :: (HasEvents n s, Monad n) => TVar (EvalEnv n s) -> IO ()
launchTimeEvents tvee = do
    now <- getCurrentTime
    --putStrLn $ "tick " ++ (show now)
    ee <- atomically $ readTVar tvee
    let timeEvents = join $ maybeToList $ runEvaluate (getTimeEvents now) ee
    unless (null timeEvents) $ putStrLn "found time event(s)"
    mapM_ (triggerTimeEvent tvee) timeEvents
    --sleep 30 second roughly
    threadDelay 30000000
    launchTimeEvents tvee

triggerTimeEvent :: (HasEvents n s, Monad n) => TVar (EvalEnv n s) -> UTCTime -> IO ()
triggerTimeEvent tvee t = do
    ee <- atomically $ readTVar tvee
    let ee' = trig t ee
    atomically $ writeTVar tvee ee'
    --save m'

trig :: (HasEvents n s, Monad n) => UTCTime -> EvalEnv n s -> EvalEnv n s
trig t ee = let ee' = execSignals (return ()) [(Signal t, t)] ee
             in ee{_evalEnv = ee'}

--execSignals :: (Show a, Show e, Typeable e, Eq e, Show d, Typeable d, Eq d, HasEvents n s) => n a -> [(Signal e d, d)] -> EvalEnv n s -> s

-- | get all events that has not been triggered yet
getTimeEvents :: (HasEvents n s) => UTCTime -> Evaluate n s [UTCTime]
getTimeEvents now = do
   eis <- use events
   times <- mapM getTimes eis
   return $ filter (\t -> t <= now && t > (-32) `addUTCTime` now) (join times)

getTimes :: EventInfo n -> Evaluate n s [UTCTime]
getTimes ei = do
  rss <- getRemainingSignals' ei
  return $ mapMaybe getTime (map snd rss)

getTime :: SomeSignal -> Maybe UTCTime
getTime (SomeSignal (Signal t)) = cast t
getTime _                    = Nothing

