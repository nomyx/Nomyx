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
import           Happstack.Server              as HS (Response, ServerPartT)
import qualified Happstack.Server              as HS (Input)
import           Language.Nomyx
import           Nomyx.Core.Engine
import           Nomyx.Core.Types              as T
import           Nomyx.Auth
import           Text.Blaze.Html5              hiding (base, map, output)
import           Text.Reform                   (CommonFormError, ErrorInputType,
                                                Form, FormError (..))
import           Text.Reform.Blaze.String      ()
import           Web.Routes.PathInfo
import           Web.Routes.RouteT
import           Web.Routes.TH                 (derivePathInfo)
import           Imprevu.Happstack.Types
import           Imprevu.Evaluation.InputEval

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
  | DoInput   Input EventNumber GameName 
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

data WebSession = WebSession {_webSession        :: TVar Session,
                              _authState         :: AuthState}

type RoutedNomyxServer a = RouteT PlayerCommand (StateT WebSession (ServerPartT IO)) a

data NomyxError = PlayerNameRequired
                | GameNameRequired
                | UniqueName
                | UniqueEmail
                | FieldTooLong Int
                | NomyxCFE (CommonFormError [HS.Input])
                deriving Show

type NomyxForm a = Form (ServerPartT IO) [HS.Input] NomyxError Html () a

data RuleTemplateForm = RuleTemplateForm {name  :: String,               -- rule name
                                          desc  :: String,               -- rule description
                                          code  :: String,               -- rule code
                                          decls :: (FilePath, FilePath)} -- list of declaration files (temp path, filename)
                                          deriving (Show)

instance PathInfo Input
instance PathInfo InputField
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

makeLenses ''WebSession
