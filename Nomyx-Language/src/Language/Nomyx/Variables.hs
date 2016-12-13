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
   getTempVar,
   VarName,
   ) where


import           Language.Nomyx.Types
import           Imprevu (V, VarName, getTempVar)
import qualified Imprevu                          as Imp
import           Data.Typeable

-- * Variables
-- | variable creation
newVar :: (Typeable a, Show a) => VarName -> a -> Nomex (Maybe (V a))
newVar = Imp.newVar

newVar_ :: (Typeable a, Show a) => VarName -> a -> Nomex (V a)
newVar_ = Imp.newVar_

newVar' :: (Typeable a, Show a) => V a -> a -> Nomex Bool
newVar' = Imp.newVar'

-- | variable reading
readVar :: (Typeable a, Show a) => V a -> Nomex (Maybe a)
readVar = Imp.readVar

readVar_ :: (Typeable a, Show a) => V a -> Nomex a
readVar_ = Imp.readVar_

-- | variable writing
writeVar :: (Typeable a, Show a) => V a -> a -> Nomex Bool
writeVar = Imp.writeVar

-- | modify a variable using the provided function
modifyVar :: (Typeable a, Show a) => V a -> (a -> a) -> Nomex Bool
modifyVar = Imp.modifyVar

-- | delete variable
delVar :: V a -> Nomex Bool
delVar = Imp.delVar

