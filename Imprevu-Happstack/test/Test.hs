
module Test where

import Imprevu.Happstack.Test as T
import Distribution.TestSuite

tests :: IO [Test]
tests = do
  T.tests
  return []
