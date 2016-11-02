{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nomyx.Client.Client
     where

import Data.Proxy
import Servant
import Servant.Client
import Nomyx.Api.Api
import Nomyx.Api.Files
import Nomyx.Client.Types
import Nomyx.Core.Serialize
import Language.Nomyx.Expression
import Control.Monad.Trans.Either
import System.FilePath

templateApi :: Proxy RuleTemplateApi
templateApi = Proxy

--getTemplate :: EitherT ServantError IO [RuleTemplate]
--postTemplate :: RuleTemplate -> EitherT ServantError IO ()
--putTemplates :: _ -> EitherT ServantError IO ()
--(getTemplate :<|> postTemplate :<|> putTemplates) = client templateApi (BaseUrl Http "localhost" 8001)

uploadTemplates :: FilePath -> Options -> IO ()
uploadTemplates yamlFile os = do
  let dir = takeDirectory yamlFile
  --ts <- readLibrary yamlFile
  --res <- runEitherT $ putTemplates ts
  --putStrLn $ show ts
  --putStrLn $ show res
  return ()


