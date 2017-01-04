{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Nomyx.Api.Server where

import           Nomyx.Api.Api
import           Servant
import           Servant
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai
import           Control.Concurrent.STM
import           Nomyx.Core.Types
import           Data.Monoid
import           Data.Typeable
import           GHC.Generics

serveApi :: TVar Session -> IO ()
serveApi tv = do
   putStrLn "Starting Api on http://localhost:8001/"
   Warp.run 8001 $ myCors $ serveWithContext nomyxApi basicAuthServerContext (server tv)

 -- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck check where
   check (BasicAuthData username password) =
     if username == "servant" && password == "server"
     then return (Authorized (User "servant"))
     else return Unauthorized

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext
                                                                                     
customPolicy :: CorsResourcePolicy
customPolicy = simpleCorsResourcePolicy { corsMethods = simpleMethods <> ["DELETE", "PUT"] }

myCors :: Middleware
myCors = cors (const $ Just customPolicy)

putSwaggerYaml :: IO ()
putSwaggerYaml = undefined --BL.putStrLn $ encode $ toSwagger nomyxApi
