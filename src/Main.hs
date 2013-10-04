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
{-# LANGUAGE GADTs, DoAndIfThenElse, ScopedTypeVariables #-}
    
module Main (main) where

import Prelude hiding ((.))
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
import Paths_Nomyx_Language as PNL
import System.Directory
import Data.Time.Clock
import Language.Nomyx hiding (getCurrentTime)
import Control.Exception as E hiding (bracket)
import Test
import Utils
import Data.Version (showVersion)
import Control.Category
import Multi
import Language.Haskell.Interpreter.Server hiding (start)
import Data.Acid (openLocalStateFrom)
import System.FilePath
import Happstack.Auth.Core.Auth (initialAuthState)
import Data.Acid.Local (createCheckpointAndClose)
import Happstack.Auth.Core.Profile (initialProfileState)
import System.Unix.Directory

defaultLogFile, profilesDir, modulesDir :: FilePath
defaultLogFile = "Nomyx.save"
profilesDir = "profiles"
modulesDir = "modules"

-- | Entry point of the program.
main :: IO Bool
main = do
   args <- getArgs 
   (flags, _) <- nomyxOpts args
   if (Version `elem` flags) then do
      putStrLn $ "Nomyx " ++ showVersion PN.version
      return True
   else if (Help `elem` flags) then do
      putStrLn $ usageInfo header options
      return True
   else do
      putStrLn "Welcome to Nomyx!"
      putStrLn "Type \"Nomyx --help\" for usage options"  
      case (Daemon `elem` flags) of
         True -> (daemonize $ start flags) >> return True
         False -> start flags >> return True

start :: [Flag] -> IO ()
start flags = do
   serverCommandUsage
   defDataDir <- PN.getDataDir
   defSourceDir <- PNL.getDataDir
   hostName <- getHostName
   logFilePath <- case (findSaveFile flags) of
      Just f -> canonicalizePath f
      Nothing -> PN.getDataFileName defaultLogFile
   let port = read $ fromMaybe "8000" (findPort flags)
   let host = fromMaybe hostName (findHost flags)
   let adminPass = fromMaybe "NXPSD" (findAdminPass flags)
   let sendMail = Mails `elem` flags
   let dataDir = fromMaybe defDataDir (findDataDir flags)
   let sourceDir = fromMaybe defSourceDir (findSourceDir flags)
   let settings = Settings logFilePath (Network host port) sendMail adminPass dataDir sourceDir
   --start the haskell interpreter
   sh <- protectHandlers $ startInterpreter dataDir
   if Test `elem` flags then do
      putStrLn $ "\nNomyx Language Tests results:\n" ++ (concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") LT.tests)
      ts <- playTests sh
      putStrLn $ "\nNomyx Game Tests results:\n" ++ (concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") ts)
      putStrLn $ "All Tests Pass: " ++ (show $ allTests && (all snd ts))
   else if (DeleteSaveFile `elem` flags) then do
      putStrLn "Deleting save files"
      (removeRecursiveSafely $ dataDir </> profilesDir)        `catch` (\(e::SomeException)-> putStrLn $ show e)
      (removeRecursiveSafely $ dataDir </> modulesDir </> "*") `catch` (\(e::SomeException)-> putStrLn $ show e)
      (removeFile (_logFilePath settings))                     `catch` (\(e::SomeException)-> putStrLn $ show e)
   else do
      --creating game structures
      multi <- case (findLoadTest flags) of
         Just testName -> loadTestName settings testName sh
         Nothing -> Main.loadMulti settings sh
      --main loop
      withAcid (Just $ dataDir </> profilesDir) $ \acid -> do
         tvSession <- atomically $ newTVar (Session sh multi acid)
         --start the web server
         forkIO $ launchWebServer tvSession (Network host port)
         forkIO $ launchTimeEvents tvSession
         serverLoop tvSession

loadMulti :: Settings -> ServerHandle -> IO Multi
loadMulti set sh = do
   fileExists <- doesFileExist $ _logFilePath $ set
   multi <- case fileExists of
      True -> do
         putStrLn $ "Loading game: " ++ (_logFilePath $ set)
         Serialize.loadMulti set sh `E.catch`
            (\e -> (putStrLn $ "Error while loading logged events, log file discarded\n" ++ (show (e::ErrorCall))) >> (return $ defaultMulti set))
      False -> return $ defaultMulti set
   return multi


-- | a loop that will handle server commands
serverLoop :: TVar Session -> IO ()
serverLoop ts = do
   s <- getLine
   case s of
      "d" -> do
         s <- atomically $ readTVar ts
         putStrLn $ displayMulti $ _multi s
         pfs <- getAllProfiles s
         putStrLn $ show pfs
      _ -> putStrLn "command not recognized"
   serverLoop ts

serverCommandUsage :: IO ()
serverCommandUsage = do
   putStrLn "Server commands:"
   putStrLn "d      -> debug"
   putStrLn "Ctrl-C -> quit"

-- | Launch mode 
data Flag = Verbose
          | Version
          | Test
          | HostName String
          | Port String
          | LogFile FilePath
          | Daemon
          | LoadTest String
          | DeleteSaveFile
          | AdminPass String
          | Mails
          | Help
          | DataDir FilePath
          | SourceDir FilePath
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v'] ["verbose"]   (NoArg Verbose)                "chatty output on stderr"
     , Option ['V'] ["version"]   (NoArg Version)                "show version number"
     , Option ['t'] ["tests"]     (NoArg Test)                   "perform routine check"
     , Option ['h'] ["host"]      (ReqArg HostName "Hostname")   "specify host name"
     , Option ['p'] ["port"]      (ReqArg Port "Port")           "specify port"
     , Option ['r'] ["read"]      (ReqArg LogFile "SaveFile")    "specify save file (default is Nomyx.save)"
     , Option ['n'] ["delete"]    (NoArg DeleteSaveFile)         "delete all save files"
     , Option ['d'] ["daemon"]    (NoArg Daemon)                 "run in daemon mode"
     , Option ['l'] ["loadtest"]  (ReqArg LoadTest "TestName")   "specify name of test to load"
     , Option ['a'] ["adminPass"] (ReqArg AdminPass "AdminPass") "specify the admin password (default is NXPSD)"
     , Option ['m'] ["mails"]     (NoArg Mails)                  "send mails (default is no)"
     , Option ['?'] ["help"]      (NoArg Help)                   "display usage options (this screen)"
     , Option ['f'] ["dataDir"]   (ReqArg DataDir "DataDir")     "set data directory"
     , Option ['s'] ["sourceDir"] (ReqArg SourceDir "SourceDir") "set source directory"
     ]
    
nomyxOpts :: [String] -> IO ([Flag], [String])
nomyxOpts argv = 
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: Nomyx [OPTION...]"

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

findAdminPass :: [Flag] -> Maybe String
findAdminPass fs = headMay $ catMaybes $ map isAdminPass fs where
    isAdminPass (AdminPass a) = Just a
    isAdminPass _ = Nothing

findDataDir :: [Flag] -> Maybe String
findDataDir fs = headMay $ catMaybes $ map isDataDir fs where
    isDataDir (DataDir a) = Just a
    isDataDir _ = Nothing

findSourceDir :: [Flag] -> Maybe String
findSourceDir fs = headMay $ catMaybes $ map isSourceDir fs where
    isSourceDir (SourceDir a) = Just a
    isSourceDir _ = Nothing

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
