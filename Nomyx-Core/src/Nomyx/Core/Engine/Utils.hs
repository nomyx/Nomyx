{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | additional tools for evaluation
module Nomyx.Core.Engine.Utils where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Text.IO as DT
import           Data.Maybe
import           Data.Typeable
import           Imprevu.Evaluation as Imp
import           Nomyx.Language.Types
import           Nomyx.Core.Engine.Types
import           Prelude                   hiding (log, (.))
import           Safe
import           Debug.Trace.Helpers    (traceM)
import           System.FilePath                 (dropFileName, (</>))
import           System.Directory (createDirectoryIfMissing)


log :: Maybe PlayerNumber -> String -> Evaluate ()
log mpn s = focusGame $ do
   time <- use currentTime
   void $ logs %= (Log mpn time s : )

focusGame :: State Game a -> Evaluate a
focusGame = lift . zoom (evalEnv . eGame)

accessGame :: Lens' Game a -> Evaluate (a, RuleNumber)
accessGame l = do
   a <- use (evalEnv . eGame . l)
   rn <- use (evalEnv . eRuleNumber)
   return (a, rn)

putGame :: Lens' Game a -> a -> Evaluate ()
putGame l a = do
   ruleActive <- evalRuleActive
   void $ (evalEnv . eGame . l) .= a --when ruleActive $

modifyGame :: Lens' Game a -> (a -> a) -> Evaluate ()
modifyGame l f = do
   ruleActive <- evalRuleActive
   void $ (evalEnv . eGame . l) %= f --when ruleActive $

evalRuleActive :: Evaluate Bool
evalRuleActive = do
   rn <- use (evalEnv . eRuleNumber)
   rs <- use (evalEnv . eGame . rules)
   return $ (rn == 0) ||
      case find (\r -> _rNumber r == rn) rs of
         Just r -> _rStatus r == Active
         Nothing -> True --TODO why should there be an evaluating rule not in the list?


--replace temporarily the rule number used for evaluation
withRN :: RuleNumber -> Evaluate a -> Evaluate a
withRN rn eval = do
   oldRn <- use (evalEnv . eRuleNumber)
   evalEnv . eRuleNumber .= rn
   a <- eval
   evalEnv . eRuleNumber .= oldRn
   return a

tracePN :: (Monad m) => Int -> String -> m ()
tracePN pn s = traceM $ "Player " ++ (show pn) ++ " " ++ s

mapStateIO :: Show s => State s a -> StateT s IO a
mapStateIO = mapStateT $ return . runIdentity

--TODO handle error cases
saveModule :: FilePath -> ModuleInfo -> IO (FilePath)
saveModule saveDir (ModuleInfo path content) = do
   let dest = saveDir </> path
   createDirectoryIfMissing True $ dropFileName dest
   DT.writeFile dest content
   return dest
