
module SimpleModule2 where

import Prelude
import Language.Nomyx

myRule :: RuleFunc
myRule = ruleFunc $ outputAll_ "Hello"
