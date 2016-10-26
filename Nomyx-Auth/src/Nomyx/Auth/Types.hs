{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nomyx.Auth.Types where

import           Control.Lens
import           Data.Acid                     (AcidState)
import           Happstack.Authenticate.Core   (AuthenticateState,
                                                AuthenticateURL (..))
import           Happstack.Server              as HS (Response, ServerPartT)
import           Web.Routes.RouteT

data AuthState = AuthState { _authenticateState :: AcidState AuthenticateState,
                             _routeAuthenticate :: AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response}

makeLenses ''AuthState
