
module SimpleModule where

import Prelude
import Nomyx.Language
import Control.Monad

myRule :: Rule
myRule = void $ outputAll_ helperFunction

helperFunction :: String
helperFunction = "Hello"
