
module Imprevu.SysMgt where

import Data.Time
import System.Random

class (Monad n) => SysMgt n where
   getCurrentTime  :: n UTCTime
   getRandomNumber :: Random a => (a, a) -> n a

