
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Rank2Types      #-}

module Imprevu.Evaluation.TimeEval where

import Imprevu.Evaluation.EventEval
import Imprevu.Event
import Data.Time
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Typeable
import Control.Lens

type Time = Signal UTCTime UTCTime

data EvalFunc n s = EvalFunc { _evalFunc     :: forall a. n a -> EvaluateN n s a,     -- evaluation function
                               _errorHandler :: EventNumber -> String -> EvaluateN n s ()}    -- error function

launchTimeEvents :: (HasEvents n s, Monad n) => TVar s -> EvalFunc n s -> IO ()
launchTimeEvents tv ef = do
    now <- getCurrentTime
    --putStrLn $ "tick " ++ (show now)
    s <- atomically $ readTVar tv
    let timeEvents = join $ maybeToList $ runEvaluate (getTimeEvents now) (EvalEnv s (_evalFunc ef) (_errorHandler ef))
    unless (null timeEvents) $ putStrLn "found time event(s)"
    mapM_ (triggerTimeEvent tv ef) timeEvents
    --sleep 30 second roughly
    threadDelay 30000000
    launchTimeEvents tv ef

triggerTimeEvent :: (HasEvents n s, Monad n) => TVar s -> EvalFunc n s -> UTCTime -> IO ()
triggerTimeEvent tv ef t = do
    s <- atomically $ readTVar tv
    let ee = EvalEnv s (_evalFunc ef) (_errorHandler ef)
    let s' = execSignals (return ()) [(Signal t, t)] ee
    atomically $ writeTVar tv s'
    --save m'

-- | get all events that has not been triggered yet
getTimeEvents :: (HasEvents n s) => UTCTime -> EvaluateN n s [UTCTime]
getTimeEvents now = do
   eis <- use events
   times <- mapM getTimes eis
   return $ filter (\t -> t <= now && t > (-32) `addUTCTime` now) (join times)

getTimes :: EventInfoN n -> EvaluateN n s [UTCTime]
getTimes ei = do
  rss <- getRemainingSignals' ei
  return $ mapMaybe getTime (map snd rss)

getTime :: SomeSignal -> Maybe UTCTime
getTime (SomeSignal (Signal t)) = cast t
getTime _                    = Nothing

