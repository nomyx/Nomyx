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
import Web.MainPage
import Control.Concurrent
import Interpret
import Control.Concurrent.STM
import Control.Monad.CatchIO (bracket)
import Language.Nomyx.Engine.Test as LT
import Data.Maybe
import Safe
import Network.BSD
import Types
import Serialize
import Paths_Nomyx as PN
import Paths_Nomyx_Language as PNL
import System.Directory (removeDirectoryRecursive, canonicalizePath, removeFile, doesFileExist)
import Data.Time.Clock
import Language.Nomyx.Engine
import Control.Exception as E hiding (bracket)
import Test
import Utils
import Data.Version (showVersion)
import Session
import Multi
import Language.Haskell.Interpreter.Server hiding (start)
import Data.Acid (openLocalStateFrom)
import System.FilePath ((</>))
import Happstack.Auth.Core.Auth (initialAuthState)
import Data.Acid.Local (createCheckpointAndClose)
import Happstack.Auth.Core.Profile (initialProfileState)
import Control.Monad.State
import System.Exit

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
      start flags
      return True

start :: [Flag] -> IO ()
start flags = do
   defDataDir <- PN.getDataDir
   defSourceDir <- PNL.getDataDir
   hostName <- getHostName
   let port = read $ fromMaybe "8000" (findPort flags)
   let host = fromMaybe hostName (findHost flags)
   let adminPass = fromMaybe "NXPSD" (findAdminPass flags)
   let sendMail = Mails `elem` flags
   -- save directory: Nomyx.save and uploaded files
   saveDir <- case (findTarFile flags) of
      Just tarFile -> untar tarFile
      Nothing -> case (findSaveDir flags) of
         Just f -> canonicalizePath f
         Nothing -> PN.getDataDir
   -- data directory: web ressources and profiles
   let dataDir = fromMaybe defDataDir (findDataDir flags)
   -- source directory: Nomyx-Language files (used only for display in GUI, since this library is statically linked otherwise)
   let sourceDir = fromMaybe defSourceDir (findSourceDir flags)
   let settings = Settings (Network host port) sendMail adminPass saveDir dataDir sourceDir

   when (Verbose `elem` flags) $ putStrLn $ "Directories:\n" ++ "save dir = " ++  saveDir ++ "\ndata dir = " ++ dataDir ++ "\nsource dir = " ++ sourceDir
   if Test `elem` flags then runTests saveDir dataDir
   else if (DeleteSaveFile `elem` flags) then cleanFile saveDir dataDir
   else do
      serverCommandUsage
      --start the haskell interpreter
      sh <- protectHandlers $ startInterpreter saveDir
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
   fileExists <- doesFileExist $ getSaveFile set
   multi <- case fileExists of
      True -> do
         putStrLn $ "Loading game: " ++ (getSaveFile set)
         Serialize.loadMulti set sh `E.catch`
            (\e -> (putStrLn $ "Error while loading logged events, log file discarded\n" ++ (show (e::ErrorCall))) >> (return $ defaultMulti set))
      False -> do
         let defMulti = defaultMulti set
         execStateT (newGame' "Default game" (GameDesc "This is the default game." "") 0 sh) defMulti
   return multi

runTests :: FilePath -> FilePath -> IO ()
runTests saveDir dataDir = do
   sh <- protectHandlers $ startInterpreter saveDir
   putStrLn $ "\nNomyx Language Tests results:\n" ++ (concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") LT.tests)
   ts <- playTests dataDir sh
   putStrLn $ "\nNomyx Game Tests results:\n" ++ (concatMap (\(a,b) -> a ++ ": " ++ (show b) ++ "\n") ts)
   let pass = allTests && (all snd ts)
   putStrLn $ "All Tests Pass: " ++ (show $ pass)
   if pass then exitSuccess else exitFailure

cleanFile :: FilePath -> FilePath -> IO ()
cleanFile saveDir dataDir = do
   putStrLn "Deleting save files"
   let catchExp io = io `catch` (\(e::SomeException)-> putStrLn $ show e)
   catchExp $ removeDirectoryRecursive $ dataDir </> profilesDir
   catchExp $ removeDirectoryRecursive $ saveDir </> uploadDir
   catchExp $ removeFile               $ saveDir </> saveFile

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
          | LoadTest String
          | DeleteSaveFile
          | AdminPass String
          | Mails
          | Help
          | SaveDir FilePath
          | DataDir FilePath
          | SourceDir FilePath
          | TarFile FilePath
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v'] ["verbose"]   (NoArg Verbose)                "chatty output on stderr"
     , Option ['V'] ["version"]   (NoArg Version)                "show version number"
     , Option ['t'] ["tests"]     (NoArg Test)                   "perform routine check"
     , Option ['h'] ["host"]      (ReqArg HostName "Hostname")   "specify host name"
     , Option ['p'] ["port"]      (ReqArg Port "Port")           "specify port"
     , Option ['n'] ["delete"]    (NoArg DeleteSaveFile)         "delete all save files"
     , Option ['l'] ["loadtest"]  (ReqArg LoadTest "TestName")   "specify name of test to load"
     , Option ['a'] ["adminPass"] (ReqArg AdminPass "AdminPass") "specify the admin password (default is NXPSD)"
     , Option ['m'] ["mails"]     (NoArg Mails)                  "send mails (default is no)"
     , Option ['?'] ["help"]      (NoArg Help)                   "display usage options (this screen)"
     , Option ['r'] ["saveDir"]   (ReqArg SaveDir "SaveDir")     "specify save directory (for Nomyx.save and uploads)"
     , Option ['f'] ["dataDir"]   (ReqArg DataDir "DataDir")     "specify data directory (for profiles and website files)"
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

findSaveDir :: [Flag] -> Maybe FilePath
findSaveDir fs = headMay $ catMaybes $ map isSaveDir fs where
    isSaveDir (SaveDir a) = Just a
    isSaveDir _ = Nothing

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

findTarFile :: [Flag] -> Maybe String
findTarFile fs = headMay $ catMaybes $ map isTarFile fs where
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

