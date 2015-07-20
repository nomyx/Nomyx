{-# LANGUAGE DeriveDataTypeable #-}

module Data.Todo where

import Control.Applicative
import Data.Typeable

-- A todo list of things left to be done before obtaining a result.
data Todo a b = Todo [a] | Done b
   deriving (Eq, Ord, Read, Show, Typeable)

instance Alternative (Todo a) where
   empty               = Todo []
   Todo as <|> Todo bs = Todo $ as ++ bs
   Todo _  <|> n       = n
   m       <|> _       = m

instance Applicative (Todo a) where
   pure                = Done
   Todo as <*> Todo bs = Todo $ as ++ bs
   Todo as <*> _       = Todo as
   Done f  <*> r       = fmap f r

instance Functor (Todo a) where
   fmap _ (Todo x) = Todo x
   fmap f (Done y) = Done $ f y

instance Monad (Todo a) where
   return = Done
   Todo as >>= _ = Todo as
   Done a >>= f = f a

toEither :: Todo a b -> Either [a] b
toEither (Todo as) = Left as
toEither (Done a)  = Right a

fromEither :: Either [a] b -> Todo a b
fromEither (Left as) = Todo as
fromEither (Right a) = Done a

toMaybe :: Todo a b -> Maybe b
toMaybe (Todo _) = Nothing
toMaybe (Done a) = Just a
