
module SimpleModule where

import Prelude
import Language.Nomyx
import Control.Monad

myRule :: Rule
myRule = void $ outputAll_ helperFunction

helperFunction :: String
helperFunction = "Hello"
