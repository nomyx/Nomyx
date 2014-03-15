{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Interpret where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server
import Language.Nomyx
import System.Directory (createDirectoryIfMissing, copyFile, removeFile, doesFileExist)
import System.FilePath ((</>), joinPath, dropExtension, takeFileName)
import Control.Exception as CE
import Data.Either.Unwrap
import Utils
import Data.List
import Control.Monad
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import qualified Mueval.Resources as MR (limitResources)
import qualified Mueval.Context as MC
import qualified Mueval.Interpreter as MI
import System.Posix.Process (nice)
import System.Posix.Resource
import System.IO.Error
import Mueval.ArgsParse

unQualImports :: [String]
unQualImports = "Language.Nomyx" :
                "Language.Nomyx.Examples" :
                "Safe" :
                "Data.Typeable" :
                "Data.Lens" :
                MC.defaultModules

qualImports :: [(String, Maybe String)]
qualImports = ("Control.Category", Just "C") : MC.qualifiedModules

defaultPackages :: [String]
defaultPackages = "Nomyx-Language" : MC.defaultPackages


exts :: [String]
exts = ["Safe", "GADTs"] ++ (map show namedExts)

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
getUploadModules :: FilePath -> IO([FilePath])
getUploadModules saveDir = do
    files <- (getUploadedModules saveDir `catch` (\(e::SomeException) -> return []))
    return $ map (\f -> joinPath [saveDir, uploadDir, f]) files

   
-- | initializes the interpreter by loading some modules.
initializeInterpreter :: FilePath -> Interpreter ()
initializeInterpreter saveDir = do
   reset -- Make sure nothing is available
   set [installedModulesInScope := False,
        languageExtensions := map MI.readExt exts]
   unsafeSetGhcOption "-fpackage-trust"
   forM_ (defaultPackages >>= words) $ \pkg -> unsafeSetGhcOption ("-trust " ++ pkg)
   uploadedMods <- liftIO $ getUploadModules saveDir
   liftIO $ putStrLn $ "Loading modules: " ++ (concat $ intersperse ", " uploadedMods)
   loadModules uploadedMods
   setTopLevelModules $ map (dropExtension . takeFileName) uploadedMods
   liftIO $ limitResources True
   let importMods = qualImports ++ zip (unQualImports) (repeat Nothing)
   setImportsQ (importMods)


---- | reads maybe a Rule out of a string.
interpretRule :: String -> ServerHandle -> IO (Either InterpreterError RuleFunc)
interpretRule s sh = (liftIO $ runIn sh $ interpret s (as :: RuleFunc))
   `catchIOError` (\(e::IOException) -> return $ Left $ NotAllowed $ "Caught exception: " ++ (show e))

getRuleFunc :: ServerHandle -> RuleCode -> IO RuleFunc
getRuleFunc sh rc = do
   res <- interpretRule rc sh
   case res of
      Right ruleFunc -> return ruleFunc
      Left e -> error $ show e

-- | check an uploaded file and reload
loadModule :: FilePath -> FilePath -> ServerHandle -> FilePath -> IO (Maybe InterpreterError)
loadModule tempModName name sh saveDir = do
   --copy the new module in the upload directory
   let dest = (saveDir </> uploadDir </> name)
   exist <- doesFileExist dest
   if exist then return $ Just $ NotAllowed "Module already uploaded"
   else do
      copyFile tempModName dest
      setMode dest
      inter <- runIn sh $ initializeInterpreter saveDir
      case inter of
         Right _ -> return $ Nothing
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

-- | Pull together several methods of reducing priority and easy access to resources:
--  'nice', and the rlimit bindings,
--  If called with False, 'limitResources' will not use POSIX rlimits.
limitResources :: Bool -> IO ()
limitResources rlimit = do nice 20 -- Set our process priority way down
                           when rlimit $ mapM_ (uncurry setResourceLimit) limits

-- | Set all the available rlimits.
--   These values have been determined through trial-and-error
stackSizeLimitSoft, stackSizeLimitHard, totalMemoryLimitSoft, totalMemoryLimitHard,
 dataSizeLimitSoft, openFilesLimitSoft, openFilesLimitHard, fileSizeLimitSoft, fileSizeLimitHard,
 dataSizeLimitHard, cpuTimeLimitSoft, cpuTimeLimitHard, coreSizeLimitSoft, coreSizeLimitHard, zero :: ResourceLimit
totalMemoryLimitSoft = dataSizeLimitSoft
totalMemoryLimitHard = dataSizeLimitHard
-- These limits seem to be useless?
stackSizeLimitSoft = zero
stackSizeLimitHard = zero
-- We allow a few files to be opened, such as package.conf, because they are necessary. This
-- doesn't seem to be security problem because it'll be opened at the module
-- stage, before code ever evaluates. I hope.
openFilesLimitSoft = openFilesLimitHard
openFilesLimitHard = ResourceLimit 70
-- TODO: It would be nice to set these to zero, but right now Hint gets around the
-- insecurity of the GHC API by writing stuff out to a file in /tmp, so we need
-- to allow our compiled binary to do file I/O... :( But at least we can still limit
-- how much we write out!
fileSizeLimitSoft = fileSizeLimitHard
fileSizeLimitHard = ResourceLimit 10800
dataSizeLimitSoft = dataSizeLimitHard
dataSizeLimitHard = ResourceLimit $ 6^(12::Int)
-- These should not be identical, to give the XCPU handler time to trigger
cpuTimeLimitSoft = ResourceLimit 10
cpuTimeLimitHard = ResourceLimit 11
coreSizeLimitSoft = coreSizeLimitHard
coreSizeLimitHard = zero


-- convenience
zero = ResourceLimit 0

limits :: [(Resource, ResourceLimits)]
limits = [--(ResourceStackSize,    ResourceLimits stackSizeLimitSoft stackSizeLimitHard),
          (ResourceTotalMemory,  ResourceLimits totalMemoryLimitSoft totalMemoryLimitHard),
          (ResourceOpenFiles,    ResourceLimits openFilesLimitSoft openFilesLimitHard),
          (ResourceFileSize,     ResourceLimits fileSizeLimitSoft fileSizeLimitHard),
          (ResourceDataSize,     ResourceLimits dataSizeLimitSoft dataSizeLimitHard),
          (ResourceCoreFileSize, ResourceLimits coreSizeLimitSoft coreSizeLimitHard),
          (ResourceCPUTime,      ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)]
