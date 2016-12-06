
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception                   as E hiding (bracket)
import           Control.Monad.State
import           Data.Maybe
import           Data.Time.Clock
import           Data.Version                        (showVersion)
import           Language.Haskell.Interpreter.Server hiding (start)
import           Network.BSD
import           Language.Nomyx
import           Nomyx.Core.Engine
import           Nomyx.Core.Engine.Evaluation
import           Nomyx.Core.Engine.Test              as LT
import           Nomyx.Core.Interpret
import           Nomyx.Core.Multi                    as Multi
import           Nomyx.Core.Profile
import           Nomyx.Core.Serialize                as Serialize
import           Nomyx.Core.Session
import           Nomyx.Core.Test
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           Nomyx.Web.MainPage
import           Nomyx.Api.Server                    (serveApi, putSwaggerYaml)
import           Paths_Nomyx                         as PN
import           Paths_Nomyx_Language                as PNL
import           Paths_Nomyx_Web                     as PNW
import           Paths_Nomyx_Library                 as PNLib
import           Prelude                             hiding ((.))
import           Safe
import           System.Console.GetOpt
import           System.Directory                    (canonicalizePath,
                                                      doesFileExist,
                                                      removeDirectoryRecursive,
                                                      removeFile)
import           System.Environment
import           System.Exit
import           System.FilePath                     ((</>))
import           Imprevu.Evaluation.TimeEval
import           Imprevu.Evaluation.EventEval

-- | Entry point of the program.
main :: IO Bool
main = do
   args <- getArgs
   (flags, _) <- nomyxOpts args
   if Version `elem` flags then putStrLn $ "Nomyx " ++ showVersion PN.version
   else if Help `elem` flags then putStrLn $ usageInfo header options
   else if API `elem` flags then putSwaggerYaml
   else do
      putStrLn "Welcome to Nomyx! test"
      putStrLn "Type \"Nomyx --help\" for usage options"
      start flags
   return True

start :: [Flag] -> IO ()
start flags = do
   defWebDir    <- PNW.getDataDir
   defSourceDir <- PNL.getDataDir
   defLib       <- PNLib.getDataFileName "templates.yaml"
   hostName     <- getHostName
   let port        = read $ fromMaybe "8000" (findPort flags)
   let host        = fromMaybe hostName      (findHost flags)
   let adminPass   = fromMaybe "NXPSD"       (findAdminPass flags)
   let sendMail    = Mails `elem` flags
   let webDir      = fromMaybe defWebDir     (findWebDir flags)    -- data directory: web ressources and profiles
   let sourceDir   = fromMaybe defSourceDir  (findSourceDir flags) -- source directory: Nomyx-Language files
   let libraryPath = fromMaybe defLib        (findLibrary flags)
   let watchdog    = fromMaybe 10 (read <$> findWatchdog flags)
   let mLoad       = findLoadTest flags
   -- save directory: Nomyx.save and uploaded files
   saveDir <- case findTarFile flags of
      Just tarFile -> untar tarFile
      Nothing -> case findSaveDir flags of
         Just f -> canonicalizePath f
         Nothing -> PN.getDataDir
   let settings    = Settings (Network host port) sendMail adminPass saveDir webDir sourceDir watchdog
   when (Verbose `elem` flags) $ putStrLn $ "Directories:\n" ++ "save dir = " ++  saveDir ++ "\nweb dir = " ++ webDir ++ "\nsource dir = " ++ sourceDir
   if Test `elem` flags
      then runTests saveDir mLoad watchdog
      else if DeleteSaveFile `elem` flags then cleanFile saveDir
      else mainLoop settings saveDir host port libraryPath


mainLoop :: Settings -> FilePath -> HostName -> Port -> FilePath -> IO ()
mainLoop settings saveDir host port lib = do
   serverCommandUsage
   --start the haskell interpreter
   sh <- protectHandlers $ startInterpreter
   --creating game structures
   multi <- Main.loadMulti settings lib sh
   --main loop
   withAcid (Just $ saveDir </> profilesDir) $ \acid -> do
     ts <- atomically $ newTVar (Session sh multi acid)
     --start the web server
     forkIO $ launchWebServer ts (Network host port)
     --forkIO $ launchTimeEvents ts (EvalFunc evalNomex' undefined)
     --start the REST API
     forkIO $ serveApi ts
     serverLoop ts

--data EvalFunc n s = EvalFunc { _evalFunc     :: forall a. n a -> EvaluateN n s a,     -- evaluation function
--                               _errorHandler :: EventNumber -> String -> EvaluateN n s ()}    -- error function
--evalNomex :: Nomex a -> Evaluate a
--
--launchTimeEvents :: (HasEvents n s, Monad n) => TVar s -> EvalFunc n s -> IO ()
--
--type Evaluate a = EvaluateN Nomex EvalState a
--
--data EvalState = EvalState { _eGame :: Game,             -- game to be read/modified
--                             _eRuleNumber :: RuleNumber} -- number of the rule requesting the evaluation

--evalNomex' :: Nomex a ->  EvaluateN Nomex Session a
--evalNomex' exp = error "evalNomex"

loadMulti :: Settings -> FilePath -> ServerHandle -> IO Multi
loadMulti set libPath sh = do
   fileExists <- doesFileExist $ getSaveFile set
   if fileExists then do
      putStrLn $ "Loading game: " ++ getSaveFile set
      Serialize.loadMulti set sh `E.catch` (errMsg set)
   else do
      lib <- readLibrary libPath
      let defMulti = defaultMulti set lib
      execStateT (newGame' "Default game" (GameDesc "This is the default game." "") 0 True sh) defMulti where

errMsg :: Settings -> ErrorCall -> IO Multi
errMsg set e = do
  putStrLn $ "Error while loading logged events, log file discarded\n" ++ show (e::ErrorCall)
  return $ defaultMulti set (Library [rAutoActivate] [])


runTests :: FilePath -> Maybe String -> Int -> IO ()
runTests saveDir mTestName delay = do
   sh <- protectHandlers $ startInterpreter
   putStrLn $ "\nNomyx Language Tests results:\n" ++ concatMap (\(a,b) -> a ++ ": " ++ show b ++ "\n") LT.tests
   ts <- playTests saveDir sh mTestName delay
   putStrLn $ "\nNomyx Game Tests results:\n" ++ concatMap (\(a,b) -> a ++ ": " ++ show b ++ "\n") ts
   let pass = allTests && all snd ts
   putStrLn $ "All Tests Pass: " ++ show pass
   if pass then exitSuccess else exitFailure

cleanFile :: FilePath -> IO ()
cleanFile saveDir = do
   putStrLn "Deleting save files"
   let catchExp io = io `catch` (\(e::SomeException)-> print e)
   catchExp $ removeDirectoryRecursive $ saveDir </> profilesDir
   catchExp $ removeDirectoryRecursive $ saveDir </> uploadDir
   catchExp $ removeDirectoryRecursive $ saveDir </> authDir
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
          | API
          | Watchdog String
          | LibraryPath FilePath
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option "v" ["verbose"]   (NoArg Verbose)                     "chatty output on stderr"
     , Option "V" ["version"]   (NoArg Version)                     "show version number"
     , Option "h" ["host"]      (ReqArg HostName "Hostname")        "specify host name"
     , Option "p" ["port"]      (ReqArg Port "Port")                "specify port"
     , Option "n" ["delete"]    (NoArg DeleteSaveFile)              "delete all save files"
     , Option "t" ["tests"]     (NoArg Test)                        "perform routine check"
     , Option "l" ["loadtest"]  (ReqArg LoadTest "TestName")        "specify name of test to load (in combination with -t i.e. -t -l \"testName\")"
     , Option "a" ["adminPass"] (ReqArg AdminPass "AdminPass")      "specify the admin password (default is NXPSD)"
     , Option "m" ["mails"]     (NoArg Mails)                       "send mails (default is no)"
     , Option "?" ["help"]      (NoArg Help)                        "display usage options (this screen)"
     , Option "r" ["saveDir"]   (ReqArg SaveDir "SaveDir")          "specify save directory (for Nomyx.save and uploads)"
     , Option "f" ["dataDir"]   (ReqArg WebDir "WebDir")            "specify data directory (for profiles and website files)"
     , Option "s" ["sourceDir"] (ReqArg SourceDir "SourceDir")      "specify source directory (for Nomyx-Language files)"
     , Option "T" ["tar"]       (ReqArg TarFile "TarFile")          "specify tar file (containing Nomyx.save and uploads)"
     , Option ""  ["api"]       (NoArg API)                         "get swagger API file"
     , Option ""  ["watchdog"]  (ReqArg Watchdog "5")               "time in seconds before killing the compilation thread"
     , Option ""  ["library"]   (ReqArg LibraryPath "Library path") "specify the path of a library of rules (yaml file)"
     ]

nomyxOpts :: [String] -> IO ([Flag], [String])
nomyxOpts argv =
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: Nomyx [OPTION...]"

findPort, findHost, findLoadTest, findSaveDir, findWebDir, findSourceDir, findAdminPass, findTarFile, findWatchdog  :: [Flag] -> Maybe String
findPort      fs = listToMaybe [a | Port          a <- fs]
findHost      fs = listToMaybe [a | HostName      a <- fs]
findLoadTest  fs = listToMaybe [a | LoadTest      a <- fs]
findSaveDir   fs = listToMaybe [a | SaveDir       a <- fs]
findWebDir    fs = listToMaybe [a | AdminPass     a <- fs]
findSourceDir fs = listToMaybe [a | WebDir        a <- fs]
findAdminPass fs = listToMaybe [a | AdminPass     a <- fs]
findTarFile   fs = listToMaybe [a | TarFile       a <- fs]
findWatchdog  fs = listToMaybe [a | Watchdog      a <- fs]
findLibrary   fs = listToMaybe [a | LibraryPath   a <- fs]
