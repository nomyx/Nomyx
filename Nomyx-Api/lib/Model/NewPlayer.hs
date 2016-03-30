{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.NewPlayer
    ( NewPlayer (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data NewPlayer = NewPlayer
    { name :: String
    } deriving (Show, Eq, Generic)

instance FromJSON NewPlayer
instance ToJSON NewPlayer
instance Arbitrary NewPlayer where
    arbitrary = NewPlayer <$> arbitrary
