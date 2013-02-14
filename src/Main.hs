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
import Web.MainPage
import Multi
import Control.Concurrent
import Interpret
import Control.Concurrent.STM
import qualified System.Posix.Signals as S
import Control.Monad.CatchIO
import Control.Monad.Trans
import Language.Nomyx.Test
import Data.Maybe
import Safe
import Network.BSD
import System.Posix.Daemonize
import Types
import Serialize
import Paths_Nomyx as PN
import Language.Haskell.Interpreter.Server hiding (start)
import System.Directory
import Data.Time.Clock

defaultLogFile :: FilePath
defaultLogFile = "Nomyx.log"

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
         case (Daemon `elem` flags) of
            True -> (daemonize $ start flags) >> return True
            False -> start flags >> return True

start :: [Flag] -> IO ()
start flags = do
         serverCommandUsage
         --start the haskell interpreter
         sh <- protectHandlers startInterpreter
         --creating game structures
         logFile <- case (findSaveFile flags) of
            Just f -> return f
            Nothing -> return defaultLogFile
         port <- case (findPort flags) of
            Just p -> return $ read p
            Nothing -> return $ 8000
         host <- case (findHost flags) of
            Just h -> return h
            Nothing -> getHostName >>= return
         --load previous state
         multi <- loadMulti logFile sh (Network host port)
         --start the web server
         forkIO $ launchWebServer multi (Network host port)
         forkIO $ launchTimeEvents multi
         --loop
         serverLoop multi logFile

loadMulti :: FilePath -> ServerHandle -> Network -> IO (TVar Multi)
loadMulti f sh net = do
   fp <- getDataFileName f
   fileExists <- doesFileExist fp
   multi <- case fileExists of
      True -> loadEvents fp sh net
      False -> return $ defaultMulti sh fp net
   atomically $ newTVar multi


-- | a loop that will handle server commands
serverLoop :: TVar Multi -> FilePath -> IO ()
serverLoop tm f = do
   s <- getLine
   case s of
      "d" -> do
         m <- atomically $ readTVar tm
         putStrLn $ show m
         serverLoop tm f
      "s" -> do
         putStrLn "saving state..."
         m <- atomically $ readTVar tm
         fp <- getDataFileName f
         save fp $ logEvents $ logs m
         serverLoop tm f
      "q" -> return ()
      _ -> do
         putStrLn "command not recognized"
         serverLoop tm f

serverCommandUsage :: IO ()
serverCommandUsage = do
   putStrLn "Server commands:"
   --putStrLn "s -> save state"
   putStrLn "d -> debug"
   putStrLn "q -> quit"

-- | Launch mode 
data Flag 
     = Verbose | Version | Test | HostName String | Port String | LogFile FilePath | Daemon
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)              "chatty output on stderr"
     , Option ['V','?'] ["version"] (NoArg Version)              "show version number"
     , Option ['t']     ["tests"]   (NoArg Test)                 "perform routine check"
     , Option ['h']     ["host"]    (ReqArg HostName "Hostname") "specify host name"
     , Option ['p']     ["port"]    (ReqArg Port "Port")         "specify port"
     , Option ['r']     ["read"]    (ReqArg LogFile "LogFile")   "specify log file"
     , Option ['d']     ["daemon"]  (NoArg Daemon)               "run in daemon mode"
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

findSaveFile :: [Flag] -> Maybe FilePath
findSaveFile fs = headMay $ catMaybes $ map isSaveFile fs where
    isSaveFile (LogFile a) = Just a
    isSaveFile _ = Nothing

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
