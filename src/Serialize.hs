
{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Serialize where

import Prelude hiding (log)
import Language.Nomyx.Expression
import Control.Monad.State
import Types
import Multi
import Language.Haskell.Interpreter.Server
import Control.Applicative
import Network.BSD

save :: FilePath -> [MultiEvent] -> IO()
save fp ges = writeFile fp $ showMultiEvents ges

save' :: StateT Multi IO ()
save' = do
   lgs <- gets logs
   lift $ save (logFilePath lgs) (logEvents lgs)

load :: FilePath -> IO([MultiEvent])
load fp = readMultiEvents <$> readFile fp

logEvent :: MultiEvent -> StateT Multi IO ()
logEvent le = do
    m <- get
    ls <- gets logs
    put m { logs = ls { logEvents = (logEvents $ logs m) ++ [le]}}


enactEvent :: MultiEvent -> StateT Multi IO ()
enactEvent (MultiNewPlayer pm)                = newPlayerU pm
enactEvent (MultiNewGame s pn)                = newGame s pn
enactEvent (MultiJoinGame gn pn)              = joinGame gn pn
enactEvent (MultiLeaveGame pn)                = leaveGame pn
enactEvent (MultiSubscribeGame gn pn)         = subscribeGame gn pn
enactEvent (MultiUnsubscribeGame gn pn)       = unsubscribeGame gn pn
enactEvent (MultiSubmitRule name text rule pn) = gets sh >>= submitRule name text rule pn
enactEvent (MultiInputChoiceResult eventNumber choiceIndex pn) = inputChoiceResult eventNumber choiceIndex pn
enactEvent (MultiInputStringResult title result pn) = inputStringResult (InputString pn title) result pn
enactEvent (MultiInputUpload pn dir mod)      = gets sh >>= inputUpload pn dir mod
enactEvent (MultiMailSettings mms pn)         = mailSettings mms pn

update :: MultiEvent -> StateT Multi IO ()
update le = logEvent le >> enactEvent le >> save'

loadEvents :: FilePath -> ServerHandle -> Network -> IO Multi
loadEvents fp sh net = execStateT (loadEvents' fp) (defaultMulti sh fp net)

loadEvents' :: FilePath -> StateT Multi IO ()
loadEvents' fp = do
   les <- liftIO $ load fp
   ls <- gets logs
   m <- get
   put m { logs = ls { logEvents = les}}
   mapM_ (\a -> (lift $ putStrLn $ "loading " ++ (show a)) >> enactEvent a) les

readMultiEvents :: String -> [MultiEvent]
readMultiEvents s = map read $ lines s

showMultiEvents :: [MultiEvent] -> String
showMultiEvents = concatMap (\a -> show a ++ "\n")
