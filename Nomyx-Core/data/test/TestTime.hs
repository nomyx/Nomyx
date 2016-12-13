
module TestTime where

import Prelude
import Nomyx.Language
import Control.Monad
import Data.Time

myRule :: Rule
myRule = void $ outputAll_ helperFunction

helperFunction :: String
helperFunction = "Hello"
