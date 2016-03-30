{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Player
    ( Player (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.NewPlayer


data Player = Player
    { name :: String
    , id_ :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON Player
instance ToJSON Player
instance Arbitrary Player where
    arbitrary = Player <$> arbitrary <*> arbitrary
