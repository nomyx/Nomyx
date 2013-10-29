
{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Serialize where

import Prelude hiding (log)
import Language.Nomyx hiding (getCurrentTime)
import Language.NomyxAPI
import Control.Monad.State
import Types
import Data.Lens
import Language.Haskell.Interpreter.Server
import Interpret
import Utils

save :: Multi -> IO ()
save m = writeFile (getSaveFile $ _mSettings m) (show m)

save' :: StateT Multi IO ()
save' = get >>= lift . save

load :: FilePath -> IO Multi
load fp = do
   s <- readFile fp
   return $ read s

loadMulti :: Settings -> ServerHandle -> IO Multi
loadMulti set sh = do
   m <- load (getSaveFile set)
   gs' <- mapM (updateLoggedGame $ getRuleFunc sh) $ _games m
   let m' = games `setL` gs' $ m
   let m'' = mSettings `setL` set $ m'
   return m''

updateLoggedGame :: (RuleCode -> IO RuleFunc) -> LoggedGame -> IO LoggedGame
updateLoggedGame f (LoggedGame g log) = getLoggedGame g f log
