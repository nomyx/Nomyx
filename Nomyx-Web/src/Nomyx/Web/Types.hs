{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nomyx.Web.Types where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.State
import           Data.Acid                     (AcidState)
import           Data.String                   (fromString)
import           Data.Text                     (Text, pack)
import           Happstack.Authenticate.Core   (AuthenticateState,
                                                AuthenticateURL (..))
import           Happstack.Server              as HS (Input, Response,
                                                      ServerPartT)
import           Language.Nomyx
import           Nomyx.Core.Engine
import           Nomyx.Core.Types              as T
import           Text.Blaze.Html5              hiding (base, map, output)
import           Text.Reform                   (CommonFormError, ErrorInputType,
                                                Form, FormError (..))
import           Text.Reform.Blaze.String      ()
import           Web.Routes.PathInfo
import           Web.Routes.RouteT
import           Web.Routes.TH                 (derivePathInfo)

default (Integer, Double, Data.Text.Text)

data GameTab = Home | Rules | Actions | Library | Details
   deriving (Show)

data PlayerCommand =
  -- Authentication and login
    Auth AuthenticateURL
  | Login
  | Logout
  | ResetPassword
  | ChangePassword
  | OpenIdRealm
  | PostAuth
  -- Game menu
  | Menu GameTab GameName
  | MainPage
  -- Game management
  | JoinGame  GameName
  | LeaveGame GameName
  | DelGame   GameName
  | NewGame
  | SubmitNewGame
  -- Game actions
  | DoInput   EventNumber SignalAddress FormField GameName
  | SubmitRule   GameName
  -- Templates
  | NewRuleTemplate GameName
  | DelRuleTemplate GameName RuleName
  -- File management
  | Upload
  --Settings
  | Advanced
  | SubmitPlayAs GameName
  | SubmitAdminPass
  | SubmitSettings
  | SaveFilePage
  -- Misc
  | NomyxJS
  deriving (Show)


data WebState = WebState {_session           :: TVar Session,
                          _authenticateState :: AcidState AuthenticateState,
                          _routeAuthenticate :: AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response}


type RoutedNomyxServer a = RouteT PlayerCommand (StateT WebState (ServerPartT IO)) a

data NomyxError = PlayerNameRequired
                | GameNameRequired
                | UniqueName
                | UniqueEmail
                | FieldTooLong Int
                | NomyxCFE (CommonFormError [HS.Input])
                deriving Show

type NomyxForm a = Form (ServerPartT IO) [HS.Input] NomyxError Html () a

instance PathInfo SignalAddressElem
instance PathInfo SignalAddress
instance PathInfo FormField
instance PathInfo (Int, String)
instance PathInfo [(Int, String)]

$(derivePathInfo ''GameTab)
$(derivePathInfo ''PlayerCommand)

instance PathInfo Bool where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "bool") (checkBool . show)
    where checkBool str = case reads str of
            [(n,[])] -> Just n
            _ ->        Nothing

instance FormError NomyxError where
    type ErrorInputType NomyxError = [HS.Input]
    commonFormError = NomyxCFE

instance ToMarkup NomyxError where
    toMarkup PlayerNameRequired = "Player Name is required"
    toMarkup GameNameRequired   = "Game Name is required"
    toMarkup UniqueName         = "Name already taken"
    toMarkup UniqueEmail        = "Email already taken"
    toMarkup (FieldTooLong l)   = fromString $ "Field max length: " ++ show l
    toMarkup (NomyxCFE e)       = fromString $ show e

makeLenses ''WebState
