
{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Serialize where

import Prelude hiding (log)
import System.IO
import Language.Nomyx.Expression
import Control.Monad.State
import Types
import Multi
import Language.Haskell.Interpreter.Server


save :: FilePath -> [MultiEvent] -> IO()
save fp ges = withFile fp WriteMode (\h -> hPutStr h $ show $ ges)

save' :: StateT Multi IO ()
save' = do
   lgs <- gets logs
   lift $ save (logFilePath lgs) (logEvents lgs)

load :: FilePath -> IO([MultiEvent])
load fp = withFile fp ReadMode (\h -> hGetLine h >>= return . read )

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
enactEvent (MultiInputUpload pn dir mod) = gets sh >>= inputUpload pn dir mod

update :: MultiEvent -> StateT Multi IO ()
update le = logEvent le >> enactEvent le >> save'

loadEvents :: FilePath -> ServerHandle-> IO Multi
loadEvents fp sh = execStateT (loadEvents' fp) (defaultMulti sh fp)

loadEvents' :: FilePath -> StateT Multi IO ()
loadEvents' fp = do
   les <- liftIO $ load fp
   ls <- gets logs
   m <- get
   put m { logs = ls { logEvents = les}}
   mapM_ (\a -> (lift $ putStrLn $ "loading " ++ (show a)) >> enactEvent a) les


--instance Show MultiEvent where
--   show a =
--   showList as = (++) $ concatMap (\a -> show a ++ "\n") as
