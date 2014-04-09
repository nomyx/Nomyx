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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
    
module Main (main) where

import Prelude hiding ((.))
import System.Console.GetOpt 
import System.Environment 
import Control.Concurrent
import Control.Concurrent.STM
import Language.Nomyx.Engine.Test as LT
import Data.Maybe
import Safe
import Network.BSD
import Paths_Nomyx as PN
import Paths_Nomyx_Web as PNW
import Paths_Nomyx_Language as PNL
import System.Directory (removeDirectoryRecursive, canonicalizePath, removeFile, doesFileExist)
import Data.Time.Clock
import Language.Nomyx.Engine
import Control.Exception as E hiding (bracket)
import Data.Version (showVersion)
import Language.Haskell.Interpreter.Server hiding (start)
import System.FilePath ((</>))
import Control.Monad.State
import System.Exit
import Nomyx.Web.MainPage
import Nomyx.Core.Profile
import Nomyx.Core.Session
import Nomyx.Core.Multi as Multi
import Nomyx.Core.Utils
import Nomyx.Core.Types
import Nomyx.Core.Serialize as Serialize
import Nomyx.Core.Interpret
import Nomyx.Core.Test


-- | Entry point of the program.
main :: IO Bool
main = do
   args <- getArgs 
   (flags, _) <- nomyxOpts args
   if Version `elem` flags then do
      putStrLn $ "Nomyx " ++ showVersion PN.version
      return True
   else if Help `elem` flags then do
      putStrLn $ usageInfo header options
      return True
   else do
      putStrLn "Welcome to Nomyx!"
      putStrLn "Type \"Nomyx --help\" for usage options"  
      start flags
      return True

start :: [Flag] -> IO ()
start flags = do
   defWebDir <- PNW.getDataDir
   defSourceDir <- PNL.getDataDir
   let defSaveDir = PN.getDataDir
   hostName <- getHostName
   let port = read $ fromMaybe "8000" (findPort flags)
   let host = fromMaybe hostName (findHost flags)
   let adminPass = fromMaybe "NXPSD" (findAdminPass flags)
   let sendMail = Mails `elem` flags
   -- save directory: Nomyx.save and uploaded files
   saveDir <- case findTarFile flags of
      Just tarFile -> untar tarFile
      Nothing -> case findSaveDir flags of
         Just f -> canonicalizePath f
         Nothing -> defSaveDir
   -- data directory: web ressources and profiles
   let webDir = fromMaybe defWebDir (findWebDir flags)
   -- source directory: Nomyx-Language files (used only for display in GUI, since this library is statically linked otherwise)
   let sourceDir = fromMaybe defSourceDir (findSourceDir flags)
   let settings = Settings (Network host port) sendMail adminPass saveDir webDir sourceDir
   let mLoad = findLoadTest flags
   when (Verbose `elem` flags) $ putStrLn $ "Directories:\n" ++ "save dir = " ++  saveDir ++ "\nweb dir = " ++ webDir ++ "\nsource dir = " ++ sourceDir
   if Test `elem` flags then runTests saveDir mLoad
   else if DeleteSaveFile `elem` flags then cleanFile saveDir
   else mainLoop settings saveDir host port


mainLoop :: Settings -> FilePath -> HostName -> Port -> IO ()
mainLoop settings saveDir host port = do
   serverCommandUsage
   --start the haskell interpreter
   sh <- protectHandlers $ startInterpreter saveDir
   --creating game structures
   multi <- Main.loadMulti settings sh
   --main loop
   withAcid (Just $ saveDir </> profilesDir) $ \acid -> do
      tvSession <- atomically $ newTVar (Session sh multi acid)
      --start the web server
      forkIO $ launchWebServer tvSession (Network host port)
      forkIO $ launchTimeEvents tvSession
      serverLoop tvSession

loadMulti :: Settings -> ServerHandle -> IO Multi
loadMulti set sh = do
   fileExists <- doesFileExist $ getSaveFile set
   if fileExists then do
      putStrLn $ "Loading game: " ++ getSaveFile set
      Serialize.loadMulti set sh `E.catch` (\e -> (putStrLn $ "Error while loading logged events, log file discarded\n" ++ (show (e::ErrorCall))) >> (return $ defaultMulti set))
   else do
      let defMulti = defaultMulti set
      execStateT (newGame' "Default game" (GameDesc "This is the default game." "") 0 True sh) defMulti


runTests :: FilePath -> Maybe String -> IO ()
runTests saveDir mTestName = do
   sh <- protectHandlers $ startInterpreter saveDir
   putStrLn $ "\nNomyx Language Tests results:\n" ++ concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") LT.tests
   ts <- playTests saveDir sh mTestName
   putStrLn $ "\nNomyx Game Tests results:\n" ++ concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") ts
   let pass = allTests && all snd ts
   putStrLn $ "All Tests Pass: " ++ show pass
   if pass then exitSuccess else exitFailure

cleanFile :: FilePath -> IO ()
cleanFile saveDir = do
   putStrLn "Deleting save files"
   let catchExp io = io `catch` (\(e::SomeException)-> print e)
   catchExp $ removeDirectoryRecursive $ saveDir </> profilesDir
   catchExp $ removeDirectoryRecursive $ saveDir </> uploadDir
   catchExp $ removeFile               $ saveDir </> saveFile

-- | a loop that will handle server commands
serverLoop :: TVar Session -> IO ()
serverLoop ts = do
   s <- getLine
   case s of
      "d" -> do
         s <- atomically $ readTVar ts
         print $ _multi s
         pfs <- getAllProfiles s
         print pfs
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
          | LoadTest String
          | DeleteSaveFile
          | AdminPass String
          | Mails
          | Help
          | SaveDir FilePath
          | WebDir FilePath
          | SourceDir FilePath
          | TarFile FilePath
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v'] ["verbose"]   (NoArg Verbose)                "chatty output on stderr"
     , Option ['V'] ["version"]   (NoArg Version)                "show version number"
     , Option ['h'] ["host"]      (ReqArg HostName "Hostname")   "specify host name"
     , Option ['p'] ["port"]      (ReqArg Port "Port")           "specify port"
     , Option ['n'] ["delete"]    (NoArg DeleteSaveFile)         "delete all save files"
     , Option ['t'] ["tests"]     (NoArg Test)                   "perform routine check"
     , Option ['l'] ["loadtest"]  (ReqArg LoadTest "TestName")   "specify name of test to load (in combination with -t i.e. -t -l \"testName\")"
     , Option ['a'] ["adminPass"] (ReqArg AdminPass "AdminPass") "specify the admin password (default is NXPSD)"
     , Option ['m'] ["mails"]     (NoArg Mails)                  "send mails (default is no)"
     , Option ['?'] ["help"]      (NoArg Help)                   "display usage options (this screen)"
     , Option ['r'] ["saveDir"]   (ReqArg SaveDir "SaveDir")     "specify save directory (for Nomyx.save and uploads)"
     , Option ['f'] ["dataDir"]   (ReqArg WebDir "WebDir")       "specify data directory (for profiles and website files)"
     , Option ['s'] ["sourceDir"] (ReqArg SourceDir "SourceDir") "specify source directory (for Nomyx-Language files)"
     , Option ['T'] ["tar"]       (ReqArg TarFile "TarFile")     "specify tar file (containing Nomyx.save and uploads)"
     ]
    
nomyxOpts :: [String] -> IO ([Flag], [String])
nomyxOpts argv = 
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: Nomyx [OPTION...]"

findPort :: [Flag] -> Maybe String
findPort fs = headMay $ mapMaybe isPort fs where
    isPort (Port a) = Just a
    isPort _ = Nothing

findHost :: [Flag] -> Maybe String
findHost fs = headMay $ mapMaybe isHost fs where
    isHost (HostName a) = Just a
    isHost _ = Nothing

findLoadTest :: [Flag] -> Maybe String
findLoadTest fs = headMay $ mapMaybe isLoadTest fs where
    isLoadTest (LoadTest a) = Just a
    isLoadTest _ = Nothing

findSaveDir :: [Flag] -> Maybe FilePath
findSaveDir fs = headMay $ mapMaybe isSaveDir fs where
    isSaveDir (SaveDir a) = Just a
    isSaveDir _ = Nothing

findAdminPass :: [Flag] -> Maybe String
findAdminPass fs = headMay $ mapMaybe isAdminPass fs where
    isAdminPass (AdminPass a) = Just a
    isAdminPass _ = Nothing

findWebDir :: [Flag] -> Maybe String
findWebDir fs = headMay $ mapMaybe isWebDir fs where
    isWebDir (WebDir a) = Just a
    isWebDir _ = Nothing

findSourceDir :: [Flag] -> Maybe String
findSourceDir fs = headMay $ mapMaybe isSourceDir fs where
    isSourceDir (SourceDir a) = Just a
    isSourceDir _ = Nothing

findTarFile :: [Flag] -> Maybe String
findTarFile fs = headMay $ mapMaybe isTarFile fs where
    isTarFile (TarFile a) = Just a
    isTarFile _ = Nothing


triggerTimeEvent :: TVar Session -> UTCTime -> IO()
triggerTimeEvent tm t = do
    (Session sh m a) <- atomically $ readTVar tm
    m' <- execWithMulti t (Multi.triggerTimeEvent t) m
    atomically $ writeTVar tm (Session sh m' a)
    save m'

launchTimeEvents :: TVar Session -> IO()
launchTimeEvents tm = do
    now <- getCurrentTime
    --putStrLn $ "tick " ++ (show now)
    (Session _ m _) <- atomically $ readTVar tm
    timeEvents <- getTimeEvents now m
    when (not $ null timeEvents) $ putStrLn "found time event(s)"
    mapM_ (Main.triggerTimeEvent tm) timeEvents
    --sleep 1 second roughly
    threadDelay 1000000
    launchTimeEvents tm

