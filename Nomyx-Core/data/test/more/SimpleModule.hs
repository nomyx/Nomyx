
module SimpleModule where

import Prelude
import Nomyx.Language
import Control.Monad

myRule2 :: Rule
myRule2 = void $ outputAll_ helperFunction

helperFunction :: String
helperFunction = "Hello"
