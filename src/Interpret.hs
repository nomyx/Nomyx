-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Interpret where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server
import Language.Nomyx
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Resource
import Control.Exception as CE
import Data.Either.Unwrap

modDir = "modules"
importList = ["Prelude",
              "Language.Nomyx",
              "GHC.Base",
              "Data.Maybe",
              "Data.List",
              "Control.Monad.State",
              "Control.Applicative",
              "Control.Monad.Error",
              "Data.Map",
              "Safe",
              "Data.Typeable",
              "Control.Category",
              "Data.Lens",
              "Control.Arrow"]
              
-- | the server handle
startInterpreter :: FilePath -> IO ServerHandle
startInterpreter dataDir = do
   sh <- start
   liftIO $ createDirectoryIfMissing True $ dataDir </> modDir
   ir <- runIn sh $ initializeInterpreter dataDir
   case ir of
      Right r -> do
         putStrLn "Interpreter Loaded"
         return $ Just r
      Left e -> error $ "sHandle: initialization error:\n" ++ show e
   return sh

-- get all uploaded modules from the directory (may be empty)
getUploadModules :: FilePath -> IO([FilePath])
getUploadModules dataDir = do
    all <- getDirectoryContents $ dataDir </> modDir
    files <- filterM (getFileStatus . (\f -> joinPath [dataDir, modDir, f]) >=> return . isRegularFile) all
    return $ map (\f -> joinPath [dataDir, modDir, f]) files

-- | initializes the interpreter by loading some modules.
initializeInterpreter :: FilePath -> Interpreter ()
initializeInterpreter dataDir = do
   fmods <- liftIO $ getUploadModules dataDir
   liftIO $ putStrLn $ "Loading modules: " ++ (concat $ intersperse ", " fmods)
   loadModules fmods
   setTopLevelModules $ map (dropExtension . takeFileName) fmods
   set [searchPath := [dataDir], languageExtensions := [GADTs, ScopedTypeVariables]] --, installedModulesInScope := False
   setImports importList
   return ()

---- | reads maybe a Rule out of a string.
interpretRule :: String -> ServerHandle -> IO (Either InterpreterError RuleFunc)
interpretRule s sh = (liftIO $ runIn sh $ interpret s (as :: RuleFunc))
   `CE.catch` (\e -> return $ Left $ NotAllowed $ "Caught exception: " ++ (show (e:: IOException)))

getRuleFunc :: ServerHandle -> RuleCode -> IO RuleFunc
getRuleFunc sh rc = do
   res <- interpretRule rc sh
   case res of
      Right ruleFunc -> return ruleFunc
      Left e -> error $ show e

--liftIO $ mapM_ (uncurry setResourceLimit) limits      
cpuTimeLimitSoft = ResourceLimit 4
cpuTimeLimitHard = ResourceLimit 5


limits :: [(Resource, ResourceLimits)]
limits = [ (ResourceCPUTime,      ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)]

-- | check an uploaded file and reload
loadModule :: FilePath -> FilePath -> ServerHandle -> FilePath -> IO (Either InterpreterError ())
loadModule tempModName name sh dataDir = do
    --copy the new module in the upload directory
    let dest = (dataDir </> modDir </> name)
    copyFile tempModName dest
    setFileMode dest (ownerModes + groupModes)
    inter <- runIn sh $ initializeInterpreter dataDir
    res <- case inter of
       Right _ -> return $ Right ()
       Left e -> do
          --suppress the faulty module
          removeFile dest
          final <- runIn sh $ initializeInterpreter dataDir
          when (isLeft final) $ putStrLn "Error: reinitialize interpreter failed"
          return $ Left e
    return res


showInterpreterError :: InterpreterError -> String
showInterpreterError (UnknownError s)  = "Unknown Error\n" ++ s
showInterpreterError (WontCompile ers) = "Won't Compile\n" ++ concatMap (\(GhcError errMsg) -> errMsg ++ "\n") ers
showInterpreterError (NotAllowed s)    = "Not Allowed (Probable cause: bad module or file name)\n" ++ s
showInterpreterError (GhcException s)  = "Ghc Exception\n" ++ s


