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
{-# LANGUAGE GADTs, DoAndIfThenElse #-}
    
module Main (main) where

import Prelude hiding (catch, (.))
import System.Console.GetOpt 
import System.Environment 
import Web.MainPage
import Control.Concurrent
import Interpret
import Control.Concurrent.STM
import qualified System.Posix.Signals as S
import Control.Monad.CatchIO hiding (catch)
import Control.Monad.Trans
import Language.Nomyx.Test as LT
import Data.Maybe
import Safe
import Network.BSD
import System.Posix.Daemonize
import Types
import Serialize
import Paths_Nomyx as PN
import System.Directory
import Data.Time.Clock
import Language.Nomyx hiding (getCurrentTime)
import Control.Exception hiding (bracket)
import Test
import Utils
import Data.Version (showVersion)
import Control.Category
import Multi
import Language.Haskell.Interpreter.Server hiding (start)
import Data.Acid (openLocalStateFrom)
import System.FilePath ((</>))
import Happstack.Auth.Core.Auth (initialAuthState)
import Data.Acid.Local (createCheckpointAndClose)
import Happstack.Auth.Core.Profile (initialProfileState)

defaultLogFile :: FilePath
defaultLogFile = "Nomyx.save"

-- | Entry point of the program.
main :: IO Bool
main = do
   args <- getArgs 
   (flags, _) <- nomyxOpts args
   if (Version `elem` flags) then do
      putStrLn $ "Nomyx " ++ showVersion version
      return True
   else do
      putStrLn "Welcome to Nomyx!" 
      case (Daemon `elem` flags) of
         True -> (daemonize $ start flags) >> return True
         False -> start flags >> return True

start :: [Flag] -> IO ()
start flags = do
   serverCommandUsage

   --start the haskell interpreter
   sh <- protectHandlers startInterpreter
   if Test `elem` flags then do
      putStrLn $ "\nNomyx Language Tests results:\n" ++ (concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") LT.tests)
      ts <- playTests sh
      putStrLn $ "\nNomyx Game Tests results:\n" ++ (concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") ts)
      putStrLn $ "All Tests Pass: " ++ (show $ allTests && (all snd ts))
   else do
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
      logFilePath <- getDataFileName logFile
      let settings sendMail = Settings logFilePath (Network host port) sendMail
      multi <- case (findLoadTest flags) of
         Just testName -> loadTestName (settings False) testName sh
         Nothing -> Main.loadMulti (settings True) (not $ NoReadSaveFile `elem` flags) sh
      --main loop
      profilePath <- getDataDir
      withAcid (Just $ profilePath </> "profiles") $ \acid -> do
         tvSession <- atomically $ newTVar (Session sh multi acid)
         --start the web server
         forkIO $ launchWebServer tvSession (Network host port)
         forkIO $ launchTimeEvents tvSession
         serverLoop tvSession logFile

loadMulti :: Settings -> Bool -> ServerHandle -> IO Multi
loadMulti set readSaveFile sh = do
   fileExists <- doesFileExist $ _logFilePath $ set
   multi <- case fileExists && readSaveFile of
      True -> do
         putStrLn "Loading previous game"
         Serialize.loadMulti set sh `catch`
            (\e -> (putStrLn $ "Error while loading logged events, log file discarded\n" ++ (show (e::ErrorCall))) >> (return $ defaultMulti set))
      False -> return $ defaultMulti set
   return multi


-- | a loop that will handle server commands
serverLoop :: TVar Session -> FilePath -> IO ()
serverLoop ts f = do
   s <- getLine
   case s of
      "d" -> do
         (Session _ m _) <- atomically $ readTVar ts
         putStrLn $ show m
         serverLoop ts f
      "s" -> do
         putStrLn "saving state..."
         (Session _ m _) <- atomically $ readTVar ts
         fp <- getDataFileName f
         save fp m
         serverLoop ts f
      "q" -> return ()
      _ -> do
         putStrLn "command not recognized"
         serverLoop ts f

serverCommandUsage :: IO ()
serverCommandUsage = do
   putStrLn "Server commands:"
   putStrLn "s -> save state"
   putStrLn "d -> debug"
   putStrLn "q -> quit"

-- | Launch mode 
data Flag 
     = Verbose | Version | Test | HostName String | Port String | LogFile FilePath | Daemon | LoadTest String | NoReadSaveFile
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"]  (NoArg Verbose)              "chatty output on stderr"
     , Option ['V','?'] ["version"]  (NoArg Version)              "show version number"
     , Option ['t']     ["tests"]    (NoArg Test)                 "perform routine check"
     , Option ['h']     ["host"]     (ReqArg HostName "Hostname") "specify host name"
     , Option ['p']     ["port"]     (ReqArg Port "Port")         "specify port"
     , Option ['r']     ["read"]     (ReqArg LogFile "SaveFile")  "specify save file (default is Nomyx.save)"
     , Option ['n']     ["noread"]   (NoArg NoReadSaveFile)       "don't read save file, just overwrite"
     , Option ['d']     ["daemon"]   (NoArg Daemon)               "run in daemon mode"
     , Option ['l']     ["loadtest"] (ReqArg LoadTest "TestName") "specify name of test to load"
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

findLoadTest :: [Flag] -> Maybe String
findLoadTest fs = headMay $ catMaybes $ map isLoadTest fs where
    isLoadTest (LoadTest a) = Just a
    isLoadTest _ = Nothing

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

triggerTimeEvent :: TVar Session -> UTCTime -> IO()
triggerTimeEvent tm t = do
    (Session sh m a) <- atomically $ readTVar tm
    m' <- execWithMulti t (Multi.triggerTimeEvent t) m
    atomically $ writeTVar tm (Session sh m' a)
    save (_mSettings >>>_logFilePath $ m') m'


launchTimeEvents :: TVar Session -> IO()
launchTimeEvents tm = do
    now <- getCurrentTime
    --putStrLn $ "tick " ++ (show now)
    (Session _ m _) <- atomically $ readTVar tm
    timeEvents <- getTimeEvents now m
    when (length timeEvents /= 0) $ putStrLn "found time event(s)"
    mapM_ (Main.triggerTimeEvent tm) timeEvents
    --sleep 1 second roughly
    threadDelay 1000000
    launchTimeEvents tm

withAcid :: Maybe FilePath -- ^ state directory
         -> (Profiles -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createCheckpointAndClose) $ \profileData ->
        f (Profiles auth profile profileData)
