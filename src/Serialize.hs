
{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Serialize where

import Prelude hiding (log, catch)
import Language.Nomyx.Expression
import Control.Monad.State
import Types
import Multi
import Language.Haskell.Interpreter.Server
import Control.Applicative
import Data.Time.Clock
import Utils
import Control.Exception

save :: FilePath -> [TimedEvent] -> IO()
save fp ges = writeFile fp $ concatMap (\a -> show a ++ "\n") ges

save' :: StateT Multi IO ()
save' = do
   lgs <- gets logs
   lift $ save (logFilePath lgs) (logEvents lgs)

load :: FilePath -> IO([TimedEvent])
load fp = (map read . lines) <$> readFile fp

logEvent :: TimedEvent -> StateT Multi IO ()
logEvent le = do
    m <- get
    ls <- gets logs
    put m { logs = ls { logEvents = (logEvents $ logs m) ++ [le]}}

putTime :: UTCTime -> StateT Multi IO ()
putTime t = modify (\m -> m{mCurrentTime = t})

enactTimedEvent :: TimedEvent -> StateT Multi IO ()
enactTimedEvent (TE t e) = do
   putTime t
   enactEvent e

enactEvent :: MultiEvent -> StateT Multi IO ()
enactEvent (MultiNewPlayer pm)                = liftT $ newPlayerU pm
enactEvent (MultiNewGame s d pn)              = liftT $ newGame s d pn
enactEvent (MultiJoinGame gn pn)              = liftT $ joinGame gn pn
enactEvent (MultiLeaveGame pn)                = liftT $ leaveGame pn
enactEvent (MultiSubscribeGame gn pn)         = liftT $ subscribeGame gn pn
enactEvent (MultiUnsubscribeGame gn pn)       = liftT $ unsubscribeGame gn pn
enactEvent (MultiSubmitRule sr pn)            = gets sh >>= submitRule sr pn
enactEvent (MultiInputChoiceResult en ci pn)  = liftT $ inputChoiceResult en ci pn
enactEvent (MultiInputStringResult ti res pn) = liftT $ inputStringResult (InputString pn ti) res pn
enactEvent (MultiInputUpload pn dir mod)      = gets sh >>= inputUpload pn dir mod
enactEvent (MultiTimeEvent t)                 = triggerTimeEvent t
enactEvent (MultiMailSettings mms pn)         = liftT $ mailSettings mms pn

update :: TimedEvent -> (Maybe PlayerNumber) -> StateT Multi IO ()
update te mpn = logEvent te >> update' te mpn >> save'

update' :: TimedEvent -> (Maybe PlayerNumber) -> StateT Multi IO ()
update' te mpn = do
   m <- get
   m' <- lift $ (enactTimedEvent' te m) `catch` commandExceptionHandler mpn m
   put m'

enactTimedEvent' te m = do
   m' <- (execStateT (enactTimedEvent te) m)
   evaluate m'
   return m'

loadEvents :: FilePath -> ServerHandle -> Network -> IO Multi
loadEvents fp sh net = do
   t <- getCurrentTime
   execStateT (loadEvents' fp) (defaultMulti sh fp net t)

loadEvents' :: FilePath -> StateT Multi IO ()
loadEvents' fp = do
   les <- liftIO $ load fp
   loadTimedEvents les

loadTimedEvents :: [TimedEvent] -> StateT Multi IO ()
loadTimedEvents les = do
   modify(\m -> m { logs = (logs m) { logEvents = les}})
   mapM_ (\a -> update' a (Just 1)) les --(lift $ putStrLn $ "loading " ++ (show a)) >>
