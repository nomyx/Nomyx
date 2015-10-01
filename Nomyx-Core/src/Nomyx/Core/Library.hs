{-# LANGUAGE TemplateHaskell    #-}

module Nomyx.Core.Library where

import Language.Nomyx
import Data.Yaml
import Data.ByteString as DB
import Nomyx.Core.Engine

-- read a library file
readLibrary :: FilePath -> IO [RuleDetails]
readLibrary fp = do
  s <- DB.readFile fp
  case decodeEither s of
     Left e -> error $ "error decoding save file: " ++ e
     Right a -> return a
