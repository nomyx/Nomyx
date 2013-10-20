
module SimpleModule where

import Prelude
import Language.Nomyx

myRule :: RuleFunc
myRule = voidRule $ outputAll' helperFunction

helperFunction :: String
helperFunction = "Hello"
