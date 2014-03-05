
module UnsafeIO where

import Prelude
import Language.Nomyx
import System.IO.Unsafe
import Control.Monad

myRule :: RuleFunc
myRule = ruleFunc $ outputAll_ $ unsafePerformIO $ readFile "/etc/passwd"
