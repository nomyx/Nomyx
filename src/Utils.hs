-----------------------------------------------------------------------------
--
-- Module      :  Utils
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Utils where

import Data.Maybe
import Data.Char
import Control.Monad.State
         
-- | this function will return just a if it can cast it to an a.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads



-- | Replaces all instances of a value in a list by another value.
replace :: Eq a => a   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)



yes = ["o", "oui", "y", "yes", "v", "vrai", "true"]
toLowerS = map toLower
isYes a = toLowerS a `elem` yes

-- | generic function to say things on transformers like GameState, ServerState etc.
say :: String -> StateT a IO ()
say = lift . putStrLn

liftT :: Show s => State s a -> StateT s IO a
liftT st = do
    s1 <- get
    let (a, s) = runState st s1
    --lift $ putStrLn $ "putting " ++ show s
    put s
    return a


