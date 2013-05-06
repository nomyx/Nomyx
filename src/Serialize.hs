
{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Serialize where

import Prelude hiding (log, catch, (.))
import Language.Nomyx hiding (getCurrentTime)
import Language.Nomyx.Game
import Control.Monad.State
import Types
import Control.Category
import Data.Lens
import Language.Haskell.Interpreter.Server
import Interpret
import Control.Applicative ((<$>))

save :: FilePath -> Multi -> IO()
save fp m = writeFile fp (show m)

save' :: StateT Multi IO ()
save' = do
   lfp <- access (mSettings >>> logFilePath)
   m <- get
   lift $ save lfp m

load :: FilePath -> IO(Multi)
load fp = do
   s <- readFile fp
   return $ read s

loadMulti :: Settings -> ServerHandle -> IO Multi
loadMulti set sh = do
   m <- load $ _logFilePath set
   gs' <- mapM (updateLoggedGame $ getRuleFunc sh) $ _games m
   let m' = games `setL` gs' $ m
   return m'


updateLoggedGame :: (RuleCode -> IO RuleFunc) -> LoggedGame -> IO LoggedGame
updateLoggedGame f (LoggedGame g log) = getLoggedGame g f log
