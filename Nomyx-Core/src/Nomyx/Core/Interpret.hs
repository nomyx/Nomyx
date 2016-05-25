{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Nomyx.Core.Interpret where

import           Control.Exception                   as CE
import           Control.Monad
import           Data.Either.Unwrap
import           Data.List
import           Data.Maybe
import           Language.Haskell.Interpreter
import           Language.Haskell.Interpreter.Server
import           Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import           Language.Nomyx
import           Nomyx.Core.Context
import           Nomyx.Core.Utils
import           System.Directory                    (copyFile,
                                                      createDirectoryIfMissing,
                                                      doesFileExist, removeFile)
import           System.FilePath                     (dropExtension, joinPath,
                                                      takeFileName, (</>))
import           System.IO.Error
import           System.IO.Temp

exts :: [String]
exts = ["Safe", "GADTs"] ++ map show namedExts

namedExts :: [Extension]
namedExts = [GADTs,
             ScopedTypeVariables,
             TypeFamilies,
             DeriveDataTypeable]

-- | the server handle
startInterpreter :: IO ServerHandle
startInterpreter = do
   sh <- start
   --liftIO $ createDirectoryIfMissing True $ saveDir </> uploadDir
   ir <- runIn sh $ initializeInterpreter Nothing
   case ir of
      Right r -> do
         putStrLn "Interpreter Loaded"
         return $ Just r
      Left e -> error $ "Interpreter initialization error:\n" ++ show e
   return sh

-- get all uploaded modules from the directory (may be empty)
getUploadModules :: FilePath -> IO [FilePath]
getUploadModules saveDir = do
    files <- getUploadedModules saveDir `catch` (\(_::SomeException) -> return [])
    return $ map (\f -> joinPath [saveDir, f]) files

-- | initializes the interpreter by loading some modules.
initializeInterpreter :: Maybe FilePath -> Interpreter ()
initializeInterpreter modDir = do
   reset
   -- Interpreter options
   set [installedModulesInScope := False,
        languageExtensions := map readExt exts]
   -- GHC options
   unsafeSetGhcOption "-w"
   unsafeSetGhcOption "-fpackage-trust"
   forM_ (defaultPackages >>= words) $ \pkg -> unsafeSetGhcOption ("-trust " ++ pkg)
   -- Modules
   when (isJust modDir) $ do
      uploadedMods <- liftIO $ getUploadModules $ fromJust modDir
      liftIO $ putStrLn $ "Loading modules: " ++ (intercalate ", " uploadedMods)
      loadModules uploadedMods
      setTopLevelModules $ map (dropExtension . takeFileName) uploadedMods
   -- Imports
   let importMods = qualImports ++ zip (unQualImports) (repeat Nothing)
   setImportsQ importMods

---- | reads maybe a Rule out of a string.
interpretRule :: ServerHandle -> RuleCode -> [Module] -> IO (Either InterpreterError Rule)
interpretRule sh rc ms = do
   dir <- createTempDirectory "/tmp" "Nomyx"
   mapM_ (copyModule dir) ms
   runIn sh $ initializeInterpreter (Just dir)
   runRule `catchIOError` handler where
      runRule = liftIO $ runIn sh $ interpret rc (as :: Rule)
      handler (e::IOException) = return $ Left $ NotAllowed $ "Caught exception: " ++ (show e)

interRule :: ServerHandle -> RuleCode -> [Module] -> IO Rule
interRule sh rc ms = do
   res <- interpretRule sh rc ms
   case res of
      Right rf -> return rf
      Left e -> error $ show e

--TODO handle error cases
copyModule :: FilePath -> Module -> IO ()
copyModule saveDir decls = do
   let dest = saveDir </> (_modPath decls)
   writeFile dest (_modContent decls)

-- | check an uploaded file and reload
--loadModule :: FilePath -> FilePath -> ServerHandle -> FilePath -> IO (Maybe InterpreterError)
--loadModule tempModName name sh saveDir = do
--   --copy the new module in the upload directory
--   let dest = saveDir </> name
--   --exist <- doesFileExist dest
--   --if exist then return $ Just $ NotAllowed "Module already uploaded"
--   --else do
--      copyFile tempModName dest
--      setMode dest
--      inter <- runIn sh $ initializeInterpreter (Just saveDir)
--      case inter of
--         Right _ -> return Nothing
--         Left e -> do
--            --suppress the faulty module
--            removeFile dest
--            final <- runIn sh $ initializeInterpreter (Just saveDir)
--            when (isLeft final) $ putStrLn "Error: reinitialize interpreter failed"
--            return $ Just e

showInterpreterError :: InterpreterError -> String
showInterpreterError (UnknownError s)  = "Unknown Error\n" ++ s
showInterpreterError (WontCompile ers) = "Won't Compile\n" ++ concatMap (\(GhcError errMsg) -> errMsg ++ "\n") ers
showInterpreterError (NotAllowed s)    = "Not Allowed (Probable cause: bad module or file name)\n" ++ s
showInterpreterError (GhcException s)  = "Ghc Exception\n" ++ s

readExt :: String -> Extension
readExt s = case reads s of
  [(e,[])] -> e
  _        -> UnknownExtension s
