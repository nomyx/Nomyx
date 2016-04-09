{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Nomyx.Api.Server where

import           Nomyx.Api.Api
import           Nomyx.Api.DefaultApi
import           Servant
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Concurrent.STM
import           Nomyx.Core.Types
import           Data.Yaml                           ( encode)
import qualified Data.ByteString.Char8            as BL
import           Servant.Swagger

serveApi :: TVar Session -> IO ()
serveApi tv = do
   putStrLn "Starting Api..."
   Warp.run 8080 $ serve api $ server tv

saveSwaggerYaml :: IO ()
saveSwaggerYaml = BL.writeFile "swagger.yaml" (encode $ toSwagger api)
