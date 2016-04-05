{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Nomyx.Api.Api (
      api
    , API
    ) where

import Nomyx.Api.DefaultApi (DefaultApi)
import Data.Proxy
import Servant.API
import Test.QuickCheck
import qualified Data.Map as Map
import Nomyx.Api.Utils

type API = DefaultApi

api :: Proxy API
api = Proxy
