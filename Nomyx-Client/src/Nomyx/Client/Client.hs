{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Nomyx.Client.Client
     where

import Data.Proxy
import Servant
import Servant.Client
import Nomyx.Api.Api
import Nomyx.Api.Files
import Nomyx.Client.Types
import Nomyx.Core.Serialize
import Nomyx.Language.Types
import Nomyx.Core.Types
import Control.Monad.Trans.Either
import System.FilePath

templateApi :: Proxy RuleTemplateApi
templateApi = Proxy

getLibrary :: EitherT ServantError IO Library 
putLibrary :: Library -> EitherT ServantError IO ()
(getLibrary :<|> putLibrary) = client templateApi (BaseUrl Http "localhost" 8001)

uploadLibrary :: FilePath -> Options -> IO ()
uploadLibrary yamlFile os = do
  let dir = takeDirectory yamlFile
  l <- readLibrary yamlFile
  res <- runEitherT $ putLibrary l
  return ()

downloadLibrary :: FilePath -> Options -> IO ()
downloadLibrary yamlFile os = do
  el <- runEitherT getLibrary
  case el of
    Right lib -> writeLibrary yamlFile lib
    Left e -> putStrLn (show e)
  return ()

