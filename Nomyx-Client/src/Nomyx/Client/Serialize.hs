{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Nomyx.Client.Serialize where

import           Data.Yaml                           (decodeEither, encode)
import qualified Data.ByteString.Char8            as BL
import           Language.Nomyx                      hiding (getCurrentTime)
import           Nomyx.Client.Types
import           System.FilePath                     ((</>))
import           Control.Lens

-- read a library file
readLibrary :: FilePath -> FilePath -> IO [RuleTemplate]
readLibrary yamlFile modBaseDir = do
  s <- BL.readFile yamlFile
  case decodeEither s of
     Left e -> error $ "error decoding library: " ++ e
     Right ts -> mapM  (getTemplate modBaseDir) ts

getTemplate :: FilePath -> TemplateView -> IO RuleTemplate
getTemplate basePath (TemplateView n d c a p cs ds) = do
  ms <- mapM (readModule basePath) ds
  return $ RuleTemplate n d c a p cs ms

readModule :: FilePath -> FilePath -> IO Module
readModule basePath mod = do
  let absPath = basePath </> mod
  content <- readFile absPath
  return $ Module mod content
