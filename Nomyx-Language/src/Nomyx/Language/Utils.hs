
module Nomyx.Language.Utils where

import Data.Typeable
import Control.Monad

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '<' : (show . typeOf) e ++ ">"

enumAll :: (Enum a, Show a, Bounded a) => [(a, String)]
enumAll = map (\a -> (a, show a)) (enumFrom minBound)
