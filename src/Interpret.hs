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
   h <- start
   liftIO $ createDirectoryIfMissing True $ dataDir </> modDir
   ir <- runIn h $ initializeInterpreter dataDir
   case ir of
      Right r -> do
         putStrLn "Interpreter Loaded"
         return $ Just r
      Left e -> error $ "sHandle: initialization error:\n" ++ show e
   return h

getUploadModules :: FilePath -> IO([FilePath])
getUploadModules dataDir = do
    all <- getDirectoryContents $ dataDir </> modDir
    files <- filterM (getFileStatus . (\f -> joinPath [dataDir, modDir, f]) >=> return . isRegularFile) all
    return $ map (\f -> joinPath [dataDir, modDir, f]) files

-- | initializes the interpreter by loading some modules.
initializeInterpreter :: FilePath -> Interpreter ()
initializeInterpreter dataDir = do
   fmods <- liftIO $ getUploadModules dataDir
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
    c <- checkModule tempModName sh dataDir
    case c of
        Right _ -> do
            let dest = (dataDir </> modDir </> name)
            copyFile tempModName dest
            setFileMode dest (ownerModes + groupModes)
            runIn sh $ initializeInterpreter dataDir
            return $ Right ()
        Left e -> do
            runIn sh $ initializeInterpreter dataDir
            return $ Left e

---- | check if a module is valid. Context will be reset.
checkModule :: FilePath -> ServerHandle -> FilePath -> IO (Either InterpreterError ())
checkModule dir sh dataDir = runIn sh $ do
   fmods <- liftIO $ getUploadModules dataDir
   liftIO $ putStrLn $ concat $ fmods
   loadModules (dir:fmods)

showInterpreterError :: InterpreterError -> String
showInterpreterError (UnknownError s)  = "Unknown Error\n" ++ s
showInterpreterError (WontCompile ers) = "Won't Compile\n" ++ concatMap (\(GhcError errMsg) -> errMsg ++ "\n") ers
showInterpreterError (NotAllowed s)    = "Not Allowed\n" ++ s
showInterpreterError (GhcException s)  = "Ghc Exception\n" ++ s


