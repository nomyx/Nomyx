
module Imprevu.SysMgt where

import           Control.Applicative hiding (Const)
import           Control.Lens
import           Control.Monad.Error
import           Control.Shortcut
import           Data.Data           (Data)
import           Data.Time
import           Data.List
import           Data.Typeable
import           GHC.Generics
import           System.Random
import           Imprevu.Events
import           Imprevu.Internal.EventEval
import           Imprevu.Internal.Utils
import           Control.Monad.State hiding (execState)
import           Control.Lens
import           Data.Todo
import           System.Random


class (Monad n) => SysMgt n where
   currentTime     :: n UTCTime
   getRandomNumber :: Random a => (a, a) -> n a

