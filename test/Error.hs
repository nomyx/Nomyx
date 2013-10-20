
module SimpleModule2 where

import Prelude
import Language.Nomyx

myfunc :: RuleFunc
myfun = voidRule $ outputAll' "Hello"
