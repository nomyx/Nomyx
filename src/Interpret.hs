-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Interpret where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server
import Language.Nomyx
import System.Directory
import System.FilePath
import Control.Exception as CE
import Data.Either.Unwrap
import Utils
import Data.List
import Control.Monad

importList = ["Prelude",
              "Language.Nomyx",
              "Language.Nomyx.Examples",
              "GHC.Base",
              "Data.Maybe",
              "Data.List",
              "Control.Monad.State",
              "Control.Monad.Reader",
              "Control.Applicative",
              "Control.Monad.Error",
              --"Data.Map",
              "Safe",
              "Data.Typeable",
              --"Control.Category",
              "Data.Lens",
              "Control.Arrow",
              "Data.Array",
              "Data.Char"]
              
-- | the server handle
startInterpreter :: FilePath -> IO ServerHandle
startInterpreter saveDir = do
   sh <- start
   liftIO $ createDirectoryIfMissing True $ saveDir </> uploadDir
   ir <- runIn sh $ initializeInterpreter saveDir
   case ir of
      Right r -> do
         putStrLn "Interpreter Loaded"
         return $ Just r
      Left e -> error $ "sHandle: initialization error:\n" ++ show e
   return sh

-- get all uploaded modules from the directory (may be empty)
getUploadModules :: FilePath -> IO([FilePath])
getUploadModules saveDir = do
    files <- getUploadedModules saveDir
    return $ map (\f -> joinPath [saveDir, uploadDir, f]) files

   
-- | initializes the interpreter by loading some modules.
initializeInterpreter :: FilePath -> Interpreter ()
initializeInterpreter saveDir = do
   fmods <- liftIO $ getUploadModules saveDir
   liftIO $ putStrLn $ "Loading modules: " ++ (concat $ intersperse ", " fmods)
   loadModules fmods
   setTopLevelModules $ map (dropExtension . takeFileName) fmods
   set [searchPath := [saveDir], languageExtensions := [GADTs, ScopedTypeVariables]] --, installedModulesInScope := False
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

-- | check an uploaded file and reload
loadModule :: FilePath -> FilePath -> ServerHandle -> FilePath -> IO (Either InterpreterError ())
loadModule tempModName name sh saveDir = do
    --copy the new module in the upload directory
    let dest = (saveDir </> uploadDir </> name)
    copyFile tempModName dest
    setMode dest
    inter <- runIn sh $ initializeInterpreter saveDir
    res <- case inter of
       Right _ -> return $ Right ()
       Left e -> do
          --suppress the faulty module
          removeFile dest
          final <- runIn sh $ initializeInterpreter saveDir
          when (isLeft final) $ putStrLn "Error: reinitialize interpreter failed"
          return $ Left e
    return res


showInterpreterError :: InterpreterError -> String
showInterpreterError (UnknownError s)  = "Unknown Error\n" ++ s
showInterpreterError (WontCompile ers) = "Won't Compile\n" ++ concatMap (\(GhcError errMsg) -> errMsg ++ "\n") ers
showInterpreterError (NotAllowed s)    = "Not Allowed (Probable cause: bad module or file name)\n" ++ s
showInterpreterError (GhcException s)  = "Ghc Exception\n" ++ s


