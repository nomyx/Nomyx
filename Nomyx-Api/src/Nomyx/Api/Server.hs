{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Nomyx.Api.Server where

import           Nomyx.Api.Api
import           Nomyx.Core.Types
import           Nomyx.Core.Profile
import           Nomyx.Language.Types
import           Servant
import           Servant
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Monoid
import           Data.Typeable
import           Data.ByteString.Char8 hiding (putStrLn)
import           GHC.Generics

serveApi :: TVar Session -> IO ()
serveApi tv = do
   putStrLn "Starting Api on http://localhost:8001/"
   Warp.run 8001 $ myCors $ serveWithContext nomyxApi (basicAuthServerContext tv) (server tv)

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: (TVar Session) -> BasicAuthCheck PlayerNumber
authCheck tv = BasicAuthCheck (authCheck' tv)

authCheck' :: (TVar Session) -> BasicAuthData -> IO (BasicAuthResult PlayerNumber)
authCheck' tv (BasicAuthData username password) = do
  s <- atomically $ readTVar tv
  mpn <- getPlayerNumber' (unpack username) s
  case mpn of
    Just pn -> return (Authorized pn)
    Nothing -> return Unauthorized

basicAuthServerContext :: TVar Session -> Context (BasicAuthCheck PlayerNumber ': '[])
basicAuthServerContext tv = (authCheck tv) :. EmptyContext
                                                                                     
customPolicy :: CorsResourcePolicy
customPolicy = simpleCorsResourcePolicy { corsMethods = simpleMethods <> ["DELETE", "PUT"] }

myCors :: Middleware
myCors = cors (const $ Just customPolicy)

putSwaggerYaml :: IO ()
putSwaggerYaml = undefined --BL.putStrLn $ encode $ toSwagger nomyxApi
