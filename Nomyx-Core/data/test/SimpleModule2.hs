
module SimpleModule2 where

import Prelude
import Nomyx.Language
import Control.Monad

myRule :: Rule
myRule = void $ outputAll_ "Hello"
