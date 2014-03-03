
module SimpleModule where

import Prelude
import Language.Nomyx

myRule :: RuleFunc
myRule = ruleFunc $ outputAll_ helperFunction

helperFunction :: String
helperFunction = "Hello"
