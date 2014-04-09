{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Nomyx.Core.Interpret where


import System.Directory (createDirectoryIfMissing, copyFile, removeFile, doesFileExist)
import System.FilePath ((</>), joinPath, dropExtension, takeFileName)
import System.IO.Error
import Data.Either.Unwrap
import Data.List
import Control.Exception as CE
import Control.Monad
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import Language.Haskell.Interpreter.Server
import Language.Nomyx
import Nomyx.Core.Context
import Nomyx.Core.Utils

exts :: [String]
exts = ["Safe", "GADTs"] ++ map show namedExts

namedExts :: [Extension]
namedExts = [GADTs,
             ScopedTypeVariables,
             TypeFamilies,
             DeriveDataTypeable]
                                
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
getUploadModules :: FilePath -> IO [FilePath]
getUploadModules saveDir = do
    files <- getUploadedModules saveDir `catch` (\(_::SomeException) -> return [])
    return $ map (\f -> joinPath [saveDir, uploadDir, f]) files
   
-- | initializes the interpreter by loading some modules.
initializeInterpreter :: FilePath -> Interpreter ()
initializeInterpreter saveDir = do
   reset -- Make sure nothing is available
   set [installedModulesInScope := False,
        languageExtensions := map readExt exts]
   unsafeSetGhcOption "-fpackage-trust"
   forM_ (defaultPackages >>= words) $ \pkg -> unsafeSetGhcOption ("-trust " ++ pkg)
   uploadedMods <- liftIO $ getUploadModules saveDir
   liftIO $ putStrLn $ "Loading modules: " ++ (intercalate ", " uploadedMods)
   loadModules uploadedMods
   setTopLevelModules $ map (dropExtension . takeFileName) uploadedMods
   let importMods = qualImports ++ zip (unQualImports) (repeat Nothing)
   setImportsQ importMods

---- | reads maybe a Rule out of a string.
interpretRule :: String -> ServerHandle -> IO (Either InterpreterError Rule)
interpretRule s sh = (liftIO $ runIn sh $ interpret s (as :: Rule))
   `catchIOError` (\(e::IOException) -> return $ Left $ NotAllowed $ "Caught exception: " ++ (show e))

getRuleFunc :: ServerHandle -> RuleCode -> IO Rule
getRuleFunc sh rc = do
   res <- interpretRule rc sh
   case res of
      Right rf -> return rf
      Left e -> error $ show e

-- | check an uploaded file and reload
loadModule :: FilePath -> FilePath -> ServerHandle -> FilePath -> IO (Maybe InterpreterError)
loadModule tempModName name sh saveDir = do
   --copy the new module in the upload directory
   let dest = saveDir </> uploadDir </> name
   exist <- doesFileExist dest
   if exist then return $ Just $ NotAllowed "Module already uploaded"
   else do
      copyFile tempModName dest
      setMode dest
      inter <- runIn sh $ initializeInterpreter saveDir
      case inter of
         Right _ -> return Nothing
         Left e -> do
            --suppress the faulty module
            removeFile dest
            final <- runIn sh $ initializeInterpreter saveDir
            when (isLeft final) $ putStrLn "Error: reinitialize interpreter failed"
            return $ Just e

showInterpreterError :: InterpreterError -> String
showInterpreterError (UnknownError s)  = "Unknown Error\n" ++ s
showInterpreterError (WontCompile ers) = "Won't Compile\n" ++ concatMap (\(GhcError errMsg) -> errMsg ++ "\n") ers
showInterpreterError (NotAllowed s)    = "Not Allowed (Probable cause: bad module or file name)\n" ++ s
showInterpreterError (GhcException s)  = "Ghc Exception\n" ++ s

readExt :: String -> Extension
readExt s = case reads s of
  [(e,[])] -> e
  _        -> UnknownExtension s
