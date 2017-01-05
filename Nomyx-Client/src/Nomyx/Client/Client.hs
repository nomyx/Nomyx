{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nomyx.Client.Client
     where

import Data.Proxy
import Data.ByteString.Char8 hiding (putStrLn)
import Servant
import Servant.Client
import Servant.API
import Nomyx.Api.Api
import Nomyx.Client.Types
import Nomyx.Core.Serialize
import Nomyx.Language.Types
import Nomyx.Core.Types hiding (port)
import Control.Monad.Trans.Either
import Control.Monad.Except
import System.FilePath
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)

templateApi :: Proxy RuleTemplateApi
templateApi = Proxy

getLibrary :: BasicAuthData -> Manager -> BaseUrl -> ExceptT ServantError IO Library 
putLibrary :: BasicAuthData -> Library -> Manager -> BaseUrl -> ExceptT ServantError IO ()
(getLibrary :<|> putLibrary) = client templateApi

uploadLibrary :: FilePath -> Options -> IO ()
uploadLibrary yamlFile (Options _ _ _ host port login password) = do
  let dir = takeDirectory yamlFile
  l <- readLibrary yamlFile
  manager <- newManager defaultManagerSettings
  let auth = BasicAuthData (pack login) (pack password)
  runExceptT (putLibrary auth l manager (BaseUrl Http host port ""))
  return ()

downloadLibrary :: FilePath -> Options -> IO ()
downloadLibrary yamlFile (Options _ _ _ host port login password) = do
  manager <- newManager defaultManagerSettings
  let auth = BasicAuthData (pack login) (pack password)
  el <- runExceptT (getLibrary auth manager (BaseUrl Http host port ""))
  case el of
    Right lib -> writeLibrary yamlFile lib
    Left e -> putStrLn (show e)
  return ()

