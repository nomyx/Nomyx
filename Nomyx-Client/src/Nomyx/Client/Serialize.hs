{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Nomyx.Client.Serialize where

import           Data.Yaml                           (decodeEither, encode)
import           Data.Either
import qualified Data.ByteString.Char8            as BL
import           Language.Nomyx                      hiding (getCurrentTime)
import           Nomyx.Client.Types
import           Nomyx.Core.Engine.Types
import           System.FilePath                     ((</>))
import           Control.Lens
import           Control.Applicative

-- read a library file
readLibrary :: FilePath -> FilePath -> IO [RuleTemplate]
readLibrary yamlFile modBaseDir = do
  s <- BL.readFile yamlFile
  case decodeEither s of
     Left e -> error $ "error decoding library: " ++ e
     Right ts -> return ts

--getTemplate :: FilePath -> RuleTemplate -> IO RuleTemplate
--getTemplate basePath (RuleTemplate n d c a p cs ds) = do
--  ms <- mapM (readModule basePath) ds
--  return $ RuleTemplate n d c a p cs (map Right ms)

readModule :: FilePath -> FilePath -> IO ModuleInfo
readModule basePath mod = do
  let absPath = basePath </> mod
  content <- readFile absPath
  return $ ModuleInfo mod content
