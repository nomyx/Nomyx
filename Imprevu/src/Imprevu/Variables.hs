{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | All the building blocks to allow rules to build variables.
-- for example, you can create a variable with:
--do
--   newVar_ "MyMoney" (0::Int)

module Imprevu.Variables
 --  V(..),
 --  newVar, newVar_, newVar',
 --  readVar, readVar_,
 --  writeVar,
 --  modifyVar,
 --  delVar,
 --  getTempVar
    where

import Imprevu.SysMgt
import Imprevu.Events
import Data.Typeable
import Control.Monad.State
import Control.Applicative
import Control.Monad.Error
import Data.Maybe
import qualified Data.Map as M
import Data.Map hiding (map, filter, insert, mapMaybe, null)
import Data.Foldable as F (mapM_)
import Control.Monad.Loops

type VarName = String

-- | a container for a variable name and type
data V a = V {varName :: VarName} deriving Typeable

class (Monad n) => VarMgt n where
   newVar          :: (Typeable a, Show a) => VarName -> a -> n (Maybe (V a))
   readVar         :: (Typeable a, Show a) => V a -> n (Maybe a)
   writeVar        :: (Typeable a, Show a) => V a -> a -> n Bool
   delVar          :: (V a) -> n Bool

-- * Variables
-- | variable creation
newVar_ :: (Typeable a, Show a, VarMgt n, MonadError String n) => VarName -> a -> n (V a)
newVar_ s a = partial "newVar_: Variable existing" (newVar s a)

newVar' :: (Typeable a, Show a, VarMgt n) => V a -> a -> n Bool
newVar' v a = maybe False (const True) <$> (newVar (varName v) a)

-- | variable reading
readVar_ :: (Typeable a, Show a, VarMgt n, MonadError String n) => V a -> n a
readVar_ v@(V a) = partial ("readVar_: Variable \"" ++ a ++ "\" with type \"" ++ (show $ typeOf v) ++ "\" not existing") (readVar v)

-- | modify a variable using the provided function
modifyVar :: (Typeable a, Show a, VarMgt n, MonadError String n) => V a -> (a -> a) -> n Bool
modifyVar v f = writeVar v . f =<< readVar_ v

-- | get temporary variable with random name
getTempVar :: (Typeable a, Show a, VarMgt n, SysMgt n) => a -> n (V a)
getTempVar a = untilJust $ do
    r <- getRandomNumber (0, 1000000 :: Int)
    newVar ("tempVar" ++ (show r)) a
