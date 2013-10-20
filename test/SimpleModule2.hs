
module SimpleModule2 where

import Prelude
import Language.Nomyx

myfunc :: RuleFunc
myfunc = voidRule $ outputAll' "Hello"
