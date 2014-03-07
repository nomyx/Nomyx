{-# LANGUAGE DoAndIfThenElse #-}

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


unQualImports :: [String]
unQualImports = ["Prelude",
                 "Language.Nomyx",
                 "Language.Nomyx.Examples",
                 "Safe",
                 "Data.Typeable",
                 "Data.Lens",
                 "Data.Function",
                 "Control.Applicative",
                 "Control.Arrow",
                 "Control.Monad",
                 "Control.Monad.Cont",
                 "Control.Monad.Error",
                 "Control.Monad.Fix",
                 "Control.Monad.Identity",
                 "Control.Monad.Instances",
                 "Control.Monad.RWS",
                 "Control.Monad.Reader",
                 "Control.Monad.State",
                 "Control.Monad.State",
                 "Control.Monad.Writer",
                 "Data.Array",
                 "Data.Bits",
                 "Data.Bool",
                 "Data.Char",
                 "Data.Complex",
                 "Data.Dynamic",
                 "Data.Either",
                 "Data.Eq",
                 "Data.Fixed",
                 "Data.Graph",
                 "Data.Int",
                 "Data.Ix",
                 "Data.List",
                 "Data.Maybe",
                 "Data.Monoid",
                 "Data.Ord",
                 "Data.Ratio",
                 "Data.Tree",
                 "Data.Tuple",
                 "Data.Typeable",
                 "Data.Word"]

qualImports :: [(String, Maybe String)]
qualImports = [
               ("Data.ByteString", Just "BS"),
               ("Data.ByteString.Char8", Just "BSC"),
               ("Data.ByteString.Lazy", Just "BSL"),
               ("Data.ByteString.Lazy.Char8", Just "BSLC"),
               ("Data.Foldable", Just "Data.Foldable"),
               ("Data.IntMap", Just "IM"),
               ("Data.IntSet", Just "IS"),
               ("Data.Map", Just "M"),
               ("Data.Sequence", Just "Data.Sequence"),
               ("Data.Set", Just "S"),
               ("Data.Traversable", Just "Data.Traversable"),
               ("Control.Category", Just "C")]

defaultPackages :: [String]
defaultPackages = ["array",
                   "base",
                   "bytestring",
                   "containers",
                   "Nomyx-Language"]


extensions :: [Extension]
extensions = [UnknownExtension "ImplicitPrelude",
              UnknownExtension "Safe",
              GADTs,
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
    files <- getUploadedModules saveDir
    return $ map (\f -> joinPath [saveDir, uploadDir, f]) files

   
-- | initializes the interpreter by loading some modules.
initializeInterpreter :: FilePath -> Interpreter ()
initializeInterpreter saveDir = do
   reset -- Make sure nothing is available
   set [installedModulesInScope := False,
        languageExtensions := extensions]
   unsafeSetGhcOption "-fpackage-trust"
   forM_ (defaultPackages >>= words) $ \pkg -> unsafeSetGhcOption ("-trust " ++ pkg)
   uploadedMods <- liftIO $ getUploadModules saveDir
   liftIO $ putStrLn $ "Loading modules: " ++ (concat $ intersperse ", " uploadedMods)
   loadModules uploadedMods
   setTopLevelModules $ map (dropExtension . takeFileName) uploadedMods
   let importMods = qualImports ++ zip (unQualImports) (repeat Nothing)
   setImportsQ (importMods)

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


