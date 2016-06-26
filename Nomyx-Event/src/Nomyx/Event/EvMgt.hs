{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE RankNTypes             #-}

-- | This module contains the type definitions necessary to build a Nomic rule.
module Nomyx.Event.EvMgt where

import           Control.Applicative hiding (Const)
import           Control.Lens
import           Control.Monad.Error
import           Control.Shortcut
import           Data.Data           (Data)
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           System.Random
import           Nomyx.Event.Events

-- * Nomyx Expression

class (Typeable n, Monad n, Applicative n) => EvMgt n where
   --Events management
   onEvent         :: (Typeable e, Show e) => Event e -> ((EventNumber, e) -> n ()) -> n EventNumber
   delEvent        :: EventNumber -> n Bool
   getEvents       :: n [EventInfo n]
   sendMessage     :: (Typeable a, Show a) => Msg a -> a -> n ()
   currentTime     :: n UTCTime

data Msg m     = Msg String deriving (Typeable, Show, Eq)

