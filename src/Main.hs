-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (main) where

import System.Console.GetOpt 
import System.Environment 
import Web
import Multi
import Control.Concurrent
import Interpret
import System.Posix.Signals
import Control.Concurrent.STM
import qualified System.Posix.Signals as S
import Control.Monad.CatchIO
import Control.Monad.Trans

-- | Entry point of the program.
main :: IO Bool
main = do
   putStrLn "Welcome to Nomyx!"
   serverCommandUsage
   args <- getArgs 
   (flags, _) <- nomyxOpts args
   --parseActions flags
   --let verbose = Verbose `elem` flags
   if Test `elem` flags
      then return True--return allTests
      else do
         multi <- newTVarIO defaultMulti
         --start the haskell interpreter
         sh <- protectHandlers startInterpreter
         --start the web server
         forkIO $ launchWebServer sh multi
         forkIO $ launchTimeEvents multi
         --loop
         serverLoop multi
         return True

-- | a loop that will handle server commands
serverLoop :: TVar Multi -> IO ()
serverLoop tm = do
   s <- getLine
   case s of
      "d" -> do
         m <- atomically $ readTVar tm
         putStrLn $ show m
         serverLoop tm
      "s" -> do
         putStrLn "saving state..."
         --createCheckpoint c
         serverLoop tm
      "q" -> return ()
      _ -> do
         putStrLn "command not recognized"
         serverLoop tm

serverCommandUsage :: IO ()
serverCommandUsage = do
   putStrLn "Server commands:"
   putStrLn "s -> save state"
   putStrLn "q -> quit"

   

-- | Launch mode 
data Flag 
     = Verbose | Version | Solo | Test 
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
     , Option ['s']     ["solo"]    (NoArg Solo)          "start solo"
     , Option ['t']     ["tests"]   (NoArg Test)          "perform routine check"
     ]

    
nomyxOpts :: [String] -> IO ([Flag], [String])
nomyxOpts argv = 
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: Nomyx [OPTION...]"


helper :: MonadCatchIO m => S.Handler -> S.Signal -> m S.Handler
helper handler signal = liftIO $ S.installHandler signal handler Nothing

signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]

saveHandlers :: MonadCatchIO m => m [S.Handler]
saveHandlers = liftIO $ mapM (helper S.Ignore) signals

restoreHandlers :: MonadCatchIO m => [S.Handler] -> m [S.Handler]
restoreHandlers h  = liftIO . sequence $ zipWith helper h signals


protectHandlers :: MonadCatchIO m => m a -> m a
protectHandlers a = bracket saveHandlers restoreHandlers $ const a
