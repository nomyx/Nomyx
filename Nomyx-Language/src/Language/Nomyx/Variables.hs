{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | All the building blocks to allow rules to build variables.
-- for example, you can create a variable with:
--do
--   newVar_ "MyMoney" (0::Int)

module Language.Nomyx.Variables (
   V(..),
   newVar, newVar_, newVar',
   readVar, readVar_,
   writeVar,
   modifyVar,
   delVar,
   getTempVar
   ) where


import Language.Nomyx.Expression
import Language.Nomyx.Events
import Data.Typeable
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import Data.Map hiding (map, filter, insert, mapMaybe, null)
import Data.Foldable as F (mapM_)
import Control.Monad.Loops

-- * Variables
-- | variable creation
newVar :: (Typeable a, Show a) => VarName -> a -> Nomex (Maybe (V a))
newVar = NewVar

newVar_ :: (Typeable a, Show a) => VarName -> a -> Nomex (V a)
newVar_ s a = partial "newVar_: Variable existing" (newVar s a)

newVar' :: (Typeable a, Show a) => V a -> a -> Nomex Bool
newVar' v a = maybe False (const True) <$> (newVar (varName v) a)

-- | variable reading
readVar :: (Typeable a, Show a) => V a -> NomexNE (Maybe a)
readVar = ReadVar

readVar_ :: (Typeable a, Show a) => V a -> Nomex a
readVar_ v@(V a) = partial ("readVar_: Variable \"" ++ a ++ "\" with type \"" ++ (show $ typeOf v) ++ "\" not existing") (liftEffect $ readVar v)

-- | variable writing
writeVar :: (Typeable a, Show a) => V a -> a -> Nomex Bool
writeVar = WriteVar

-- | modify a variable using the provided function
modifyVar :: (Typeable a, Show a) => V a -> (a -> a) -> Nomex Bool
modifyVar v f = writeVar v . f =<< readVar_ v

-- | delete variable
delVar :: V a -> Nomex Bool
delVar = DelVar

-- | get temporary variable with random name
getTempVar :: (Typeable a, Show a) => a -> Nomex (V a)
getTempVar a = untilJust $ do
    r <- getRandomNumber (0, 1000000 :: Int)
    newVar ("tempVar" ++ (show r)) a
