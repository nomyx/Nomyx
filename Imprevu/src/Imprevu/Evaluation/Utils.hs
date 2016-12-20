
module Imprevu.Evaluation.Utils (
   (===),
   replaceWith,
   sel,
   toEither,
   toMaybe,
   module Debug.Trace) where

import Control.Lens           hiding (runIdentity)
import Control.Monad.Identity
import Control.Monad.State
import Control.Applicative
import Data.Typeable          (Typeable, cast)
import Debug.Trace            (trace)
import Data.Validation
import Data.Semigroup

-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y

-- | Replaces all instances of a value in a list by another value.
replaceWith :: (a -> Bool)   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replaceWith f y = map (\z -> if f z then y else z)

sel :: [a]   -- ^ List of indices to select
    -> [Int] -- ^ List of elements
    -> [a]   -- ^ List composed of elements selected from original set by indices provided
sel xs is = map (\i -> xs!!i) is

toMaybe :: AccValidation a b -> Maybe b
toMaybe (AccFailure _) = Nothing
toMaybe (AccSuccess a) = Just a

toEither :: AccValidation a b -> Either a b
toEither (AccFailure as) = Left as
toEither (AccSuccess a)  = Right a

instance (Monoid e, Semigroup e) => Alternative (AccValidation e) where
   empty               = AccFailure mempty
   AccFailure as <|> AccFailure bs = AccFailure $ as `mappend` bs
   AccFailure _  <|> n       = n
   m       <|> _       = m
