{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Nomyx.Api.Server where

import Nomyx.Api.Api
import Nomyx.Api.DefaultApi
import Servant
import qualified Network.Wai.Handler.Warp as Warp

serveApi :: IO ()
serveApi = Warp.run 8080 $ serve api server
