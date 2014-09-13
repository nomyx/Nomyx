
module Control.Shortcut where

import Control.Applicative
import Data.Either

-- | class of things that can be run in parallel but can be shortcuted.
-- The funtion in parameter is called everytime an intermediate result is known, as soon as it
-- returns True the intermediate results are returned (discarding those not yet available).
class Shortcutable s where
   shortcut :: [s a] -> ([a] -> Bool) -> s [a]

-- |  version with two different types of result
shortcut2 :: (Functor s, Shortcutable s) => [s a] -> [s b] -> ([a] -> [b] -> Bool) -> s ([a], [b])
shortcut2 as bs f = partitionEithers <$> shortcut ((map (Left <$>) as) ++ (map (Right <$>) bs)) f' where
   f' as = f (lefts as) (rights as)
