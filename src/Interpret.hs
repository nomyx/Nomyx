-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Interpret(startInterpreter, readNamedRule, interpretRule, loadModule) where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server
import Control.Monad()
import Paths_Nomyx
import Language.Nomyx.Expression
import System.Directory
import System.FilePath
import System.Posix.Files
import Control.Monad

modDir = "modules"

-- | the server handle
startInterpreter :: IO ServerHandle
startInterpreter = do
   h <- start
   dataDir <- liftIO getDataDir
   liftIO $ createDirectoryIfMissing True $ dataDir </> modDir
   ir <- runIn h initializeInterpreter
   case ir of
      Right r -> do
         putStrLn "Interpreter Loaded"
         return $ Just r
      Left e -> error $ "sHandle: initialization error:\n" ++ show e
   return h

getUploadModules :: IO([FilePath])
getUploadModules = do
    dataDir <- getDataDir
    all <- getDirectoryContents $ dataDir </> modDir
    files <- filterM (getFileStatus . (\f -> joinPath [dataDir, modDir, f]) >=> return . isRegularFile) all
    return $ map (\f -> joinPath [dataDir, modDir, f]) files

-- | initializes the interpreter by loading some modules.
initializeInterpreter :: Interpreter ()
initializeInterpreter = do
   fmods <- liftIO getUploadModules
   liftIO $ putStrLn $ concat $ fmods
   loadModules fmods
   setTopLevelModules $ map (dropExtension . takeFileName) fmods
   dataDir <- liftIO getDataDir
   set [searchPath := [dataDir]]
   setImports ["Prelude", "Language.Nomyx.Rule", "Language.Nomyx.Expression", "Test", "Examples", "GHC.Base", "Data.Maybe"]
   return ()

-- | reads maybe a Rule out of a string.
interpretRule :: String -> ServerHandle -> IO (Either InterpreterError RuleFunc)
interpretRule s sh = do
   liftIO $ runIn sh (interpret s (as :: RuleFunc))


-- | reads a Rule. May produce an error if badly formed.
readRule :: String -> ServerHandle -> IO RuleFunc
readRule sr sh = do
   ir <- interpretRule sr sh
   case ir of
      Right r -> return r
      Left e -> error $ "errReadRule: Rule is ill-formed. Shouldn't have happened.\n" ++ show e

-- | reads a NamedRule. May produce an error if badly formed.
readNamedRule :: Rule -> ServerHandle -> IO RuleFunc
readNamedRule r sh = readRule (rRuleCode r) sh

-- | check an uploaded file and reload
loadModule :: FilePath -> FilePath -> ServerHandle -> IO (Either InterpreterError ())
loadModule dir name sh = do
    dataDir <- getDataDir
    c <- checkModule dir sh
    case c of
        Right _ -> do
            copyFile dir (dataDir </> modDir </> name)
            runIn sh $ initializeInterpreter
            return $ Right ()
        Left e -> do
            runIn sh $ initializeInterpreter
            return $ Left e

-- | check if a module is valid. Context will be reset.
checkModule :: FilePath -> ServerHandle -> IO (Either InterpreterError ())
checkModule dir sh = runIn sh $ do
   fmods <- liftIO getUploadModules
   liftIO $ putStrLn $ concat $ fmods
   loadModules (dir:fmods)

