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
import Control.Concurrent.STM
import qualified System.Posix.Signals as S
import Control.Monad.CatchIO
import Control.Monad.Trans
import Test
import Data.Maybe
import Safe
import Network.BSD


-- | Entry point of the program.
main :: IO Bool
main = do
   putStrLn "Welcome to Nomyx!"
   args <- getArgs 
   (flags, _) <- nomyxOpts args
   --parseActions flags
   --let verbose = Verbose `elem` flags
   if Test `elem` flags
      then do
         putStrLn $ "Tests result: " ++ show allTests
         return allTests
      else do
         serverCommandUsage
         multi <- newTVarIO defaultMulti
         --start the haskell interpreter
         sh <- protectHandlers startInterpreter
         --start the web server
         port <- case (findPort flags) of
            Just p -> return $ read p
            Nothing -> return $ read "8000"
         host <- case (findHost flags) of
            Just h -> return h
            Nothing -> getHostName >>= return
         forkIO $ launchWebServer sh multi host port
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
   --putStrLn "s -> save state"
   putStrLn "d -> debug"
   putStrLn "q -> quit"

-- | Launch mode 
data Flag 
     = Verbose | Version | Test | HostName String | Port String
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
     , Option ['t']     ["tests"]   (NoArg Test)          "perform routine check"
     , Option ['h']     ["host"]    (ReqArg HostName "Hostname")      "specify host name"
     , Option ['p']     ["port"]    (ReqArg Port "Port")           "specify port"
     ]
    
nomyxOpts :: [String] -> IO ([Flag], [String])
nomyxOpts argv = 
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: Nomyx [OPTION...]"

findPort :: [Flag] -> Maybe String
findPort fs = headMay $ catMaybes $ map isPort fs where
    isPort (Port a) = Just a
    isPort _ = Nothing

findHost :: [Flag] -> Maybe String
findHost fs = headMay $ catMaybes $ map isHost fs where
    isHost (HostName a) = Just a
    isHost _ = Nothing

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
