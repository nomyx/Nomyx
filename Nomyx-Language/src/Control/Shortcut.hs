{-# LANGUAGE ScopedTypeVariables #-}

module Control.Shortcut where

import Control.Applicative
import Data.Maybe

-- | class of things that can be run in parallel and can be shortcuted.
-- The funtion in parameter is called everytime an intermediate result is known, as soon as it
-- returns True the intermediate results are returned (discarding those not yet available).
-- the order of the lists must be preserved
class Shortcutable s where
   shortcut :: [s a] -> ([Maybe a] -> Bool) -> s [Maybe a]

-- | version without the maybes
shortcut_ :: (Functor s, Shortcutable s) => [s a] -> ([a] -> Bool) -> s [a]
shortcut_ as f = catMaybes <$> shortcut as (f . catMaybes)

-- |  version with two different types of result
shortcut2 :: (Functor s, Monad s, Shortcutable s) => [s a] -> [s b] -> ([Maybe a] -> [Maybe b] -> Bool) -> s ([Maybe a], [Maybe b])
shortcut2 as bs f = do
  let stripEitherFst aes = map (fromLeft <$>) $ take (length as) aes
  let stripEitherLst bes = map (fromRight <$>) $ drop (length as) bes
  let f' abs = f (stripEitherFst abs) (stripEitherLst abs)
  mabs <- shortcut ((map (Left <$>) as) ++ (map (Right <$>) bs)) f'
  return (stripEitherFst mabs, stripEitherLst mabs)

-- |  version with a supplementary event which result is discarded
shortcut2b :: (Functor s, Monad s, Shortcutable s) => [s a] -> s b -> ([Maybe a] -> Bool -> Bool) -> s ([Maybe a], Bool)
shortcut2b as b f = do
   let f' as bs = f as (isJust $ head bs)
   (as, bs) <- shortcut2 as [b] f'
   return (as, isJust $ head bs)

fromLeft           :: Either a b -> a
fromLeft (Right _) = error "Argument takes form 'Right _'"
fromLeft (Left x)  = x

fromRight           :: Either a b -> b
fromRight (Left _)  = error "Argument takes form 'Left _'"
fromRight (Right x) = x
