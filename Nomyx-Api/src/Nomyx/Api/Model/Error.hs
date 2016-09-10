{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nomyx.Api.Model.Error
    ( Error (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Test.QuickCheck


data Error = Error
    { code :: Integer
    , message :: String
    , fields :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Error
instance ToJSON Error
instance Arbitrary Error where
    arbitrary = Error <$> arbitrary <*> arbitrary <*> arbitrary
