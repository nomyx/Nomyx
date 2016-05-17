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
import Servant.Client
import Nomyx.Api.Api


templateApi :: Proxy NomyxApi
templateApi = Proxy

getTemplate :: EitherT ServantErr IO [RuleTemplate]
postTemplate :: RuleTemplate -> EitherT ServantErr IO ()
putTemplates :: [RuleTemplate] -> EitherT ServantErr IO ()
getTemplate :<|> postTemplate :<|> putTemplates = client nomyxAPI (BaseUrl Http "hackage.haskell.org" 80)

