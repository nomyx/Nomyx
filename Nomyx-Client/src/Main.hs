
module Main where

import Nomyx.Client.Client
import           System.Environment

main :: IO ()
main = do
   args <- getArgs
   uploadTemplates $ head args
