{-# LANGUAGE DeriveDataTypeable #-}

module Data.Todo where

import Control.Applicative
import Data.Typeable

data Todo a b = Todo [a] | Done b
   deriving (Eq, Ord, Read, Show, Typeable)

instance Alternative (Todo a) where
   empty             = Todo []
   Todo a <|> Todo b = Todo $ a ++ b
   Todo _ <|> n      = n
   m      <|> _      = m

instance Applicative (Todo a) where
   pure              = Done
   Todo a <*> Todo b = Todo $ a ++ b
   Todo a <*> _      = Todo a
   Done f <*> r      = fmap f r

instance Functor (Todo a) where
   fmap _ (Todo x) = Todo x
   fmap f (Done y) = Done $ f y

toEither :: Todo a b -> Either [a] b
toEither (Todo as) = Left as
toEither (Done a)  = Right a

fromEither :: Either [a] b -> Todo a b
fromEither (Left as) = Todo as
fromEither (Right a) = Done a


