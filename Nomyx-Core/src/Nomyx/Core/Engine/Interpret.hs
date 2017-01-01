{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | This module interprets strings representing rules to convert them to plain Rules.
module Nomyx.Core.Engine.Interpret (
   interpretRule,
   interpretRule',
   showInterpreterError 
   )
   where

import           Control.Exception                   as CE
import           Control.Monad
import           Control.Monad.Catch  as MC
import           Data.List
import           Language.Haskell.Interpreter
import           Language.Haskell.Interpreter.Server
import           Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import           Nomyx.Language
import           Nomyx.Core.Engine.Context
import           Nomyx.Core.Engine.Utils
import           System.FilePath                     (dropExtension, joinPath,
                                                      takeFileName, dropFileName,
                                                      splitDirectories, takeBaseName, (</>))
import           System.IO.Temp
import           System.IO.Unsafe
import           System.Directory
import           System.Log.Logger
#ifndef WINDOWS
import qualified System.Posix.Signals as S
#endif


serverHandle :: ServerHandle
serverHandle = unsafePerformIO $ start

exts :: [String]
exts = ["Safe", "GADTs"] ++ map show namedExts

namedExts :: [Extension]
namedExts = [GADTs,
             ScopedTypeVariables,
             TypeFamilies,
             DeriveDataTypeable]

-- | initializes the interpreter by loading some modules.
initializeInterpreter :: [ModuleInfo] -> Interpreter ()
initializeInterpreter mods = do
   reset
   -- Interpreter options
   set [installedModulesInScope := False,
        languageExtensions := map readExt exts]
   -- GHC options
   unsafeSetGhcOption "-w"
   unsafeSetGhcOption "-fpackage-trust"
   forM_ (defaultPackages >>= words) $ \pkg -> unsafeSetGhcOption ("-trust " ++ pkg)
   -- Modules
   when (not $ null mods) $ do
      dir <- liftIO $ createTempDirectory "/tmp" "Nomyx"
      modPaths <- liftIO $ mapM (saveModule dir) mods
      let modNames = map (getModName . _modPath) mods
      info $ "Loading modules: " ++ (intercalate ", " modPaths)
      info $ "module names: " ++ (intercalate ", " modNames)
      loadModules modPaths
      setTopLevelModules modNames
   -- Imports
   let importMods = qualImports ++ zip (unQualImports) (repeat Nothing)
   setImportsQ importMods

getModName :: FilePath -> String
getModName fp = intercalate "." $ (filter (/= ".") $ splitDirectories $ dropFileName fp) ++ [takeBaseName fp]

---- | reads a Rule out of a string.
interpretRule :: RuleCode -> [ModuleInfo] -> IO (Either InterpreterError Rule)
interpretRule rc ms = runRule `catchIOError` handler where 
   runRule = protectHandlers $ runIn serverHandle $ do
      initializeInterpreter ms
      interpret rc (as :: Rule)
   handler (e::IOException) = return $ Left $ NotAllowed $ "Caught exception: " ++ (show e)

interpretRule' :: RuleCode -> [ModuleInfo] -> IO Rule
interpretRule' rc ms = do
   res <- interpretRule rc ms
   case res of
      Right rf -> return rf
      Left e -> error $ show e

showInterpreterError :: InterpreterError -> String
showInterpreterError (UnknownError s)  = "Unknown Error\n" ++ s
showInterpreterError (WontCompile ers) = "Won't Compile\n" ++ concatMap (\(GhcError errMsg) -> errMsg ++ "\n") ers
showInterpreterError (NotAllowed s)    = "Not Allowed (Probable cause: bad module or file name)\n" ++ s
showInterpreterError (GhcException s)  = "Ghc Exception\n" ++ s

readExt :: String -> Extension
readExt s = case reads s of
  [(e,[])] -> e
  _        -> UnknownExtension s

#ifdef WINDOWS

--no signals under windows
protectHandlers :: IO a -> IO a
protectHandlers = id

#else

installHandler' :: S.Handler -> S.Signal -> IO S.Handler
installHandler' handler signal = S.installHandler signal handler Nothing

signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]

saveHandlers :: IO [S.Handler]
saveHandlers = liftIO $ mapM (installHandler' S.Ignore) signals

restoreHandlers :: [S.Handler] -> IO [S.Handler]
restoreHandlers h  = liftIO . sequence $ zipWith installHandler' h signals

protectHandlers :: IO a -> IO a
protectHandlers a = MC.bracket saveHandlers restoreHandlers $ const a

#endif

warn, info :: (MonadIO m) => String -> m ()
info s = liftIO $ infoM "Nomyx.Core.Engine.Interpret" s
warn s = liftIO $ warningM "Nomyx.Core.Engine.Interpret" s
