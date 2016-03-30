{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Apis (
      api
    , API
    ) where

import Api.DefaultApi (DefaultApi)

import Data.Proxy
import Servant.API
import Test.QuickCheck
import qualified Data.Map as Map
import Utils

type API = DefaultApi

api :: Proxy API
api = Proxy
