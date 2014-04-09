
module SimpleModule2 where

import Prelude
import Language.Nomyx
import Control.Monad

myRule :: Rule
myRule = void $ outputAll_ "Hello"
