-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Interpret(startInterpreter, readNamedRule, interpretRule) where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server
import Control.Monad()
import Paths_Nomyx
import Language.Nomyx.Expression

-- | the server handle
startInterpreter :: IO ServerHandle
startInterpreter = do
   h <- start
   ir <- runIn h initializeInterpreter
   case ir of
      Right r -> do
         putStrLn "Interpreter Loaded"
         return $ Just r
      Left e -> error $ "sHandle: initialization error:\n" ++ show e
   return h

-- | initializes the interpreter by loading some modules.
initializeInterpreter :: Interpreter ()
initializeInterpreter = do
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
