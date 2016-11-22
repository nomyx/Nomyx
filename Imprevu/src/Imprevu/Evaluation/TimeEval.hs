
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Rank2Types      #-}

module Imprevu.Evaluation.TimeEval where

import Imprevu.Types
import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.Types
import Data.Time
import Data.Maybe
import Data.Typeable
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Lens

type Time = Signal UTCTime UTCTime

launchTimeEvents :: (Monad n) => TVar s -> EvalConfN n s -> IO ()
launchTimeEvents tv ec = do
    now <- getCurrentTime
    --putStrLn $ "tick " ++ (show now)
    s <- atomically $ readTVar tv
    let timeEvents = join $ maybeToList $ runEvaluate (getTimeEvents now) (EvalEnv s ec)
    unless (null timeEvents) $ putStrLn "found time event(s)"
    mapM_ (triggerTimeEvent tv ec) timeEvents
    --sleep 30 second roughly
    threadDelay 30000000
    launchTimeEvents tv ec

triggerTimeEvent :: (Monad n) => TVar s -> EvalConfN n s -> UTCTime -> IO ()
triggerTimeEvent tv ec t = do
    s <- atomically $ readTVar tv
    let s' = execSignals (return ()) [(Signal t, t)] (EvalEnv s ec)
    atomically $ writeTVar tv s'
    --save m'

-- | get all events that has not been triggered yet
getTimeEvents :: UTCTime -> EvaluateN n s [UTCTime]
getTimeEvents now = do
   eis <- use events
   times <- mapM getTimes eis
   return $ filter (\t -> t <= now && t > (-32) `addUTCTime` now) (join times)

getTimes :: EventInfoN n -> EvaluateN n s [UTCTime]
getTimes ei = do
  rss <- getRemainingSignals' ei
  return $ mapMaybe getTime rss

getTime :: SomeSignal -> Maybe UTCTime
getTime (SomeSignal (Signal t)) = cast t
getTime _                    = Nothing

