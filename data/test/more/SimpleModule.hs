
module SimpleModule where

import Prelude
import Language.Nomyx

myRule2 :: RuleFunc
myRule2 = ruleFunc $ outputAll_ helperFunction

helperFunction :: String
helperFunction = "Hello"
