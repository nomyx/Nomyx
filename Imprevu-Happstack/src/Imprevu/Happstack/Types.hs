{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ExistentialQuantification    #-}

module Imprevu.Happstack.Types where

import Imprevu.Internal.Event
import Imprevu.Inputs
import Imprevu.Internal.EventEval
import Imprevu.Internal.InputEval
import Imprevu.Events
import Imprevu.Inputs
import Control.Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Applicative
import Control.Concurrent.STM
import Data.Monoid
import Data.Maybe
import Data.Typeable
import Data.String
import           Control.Monad.State
import Happstack.Server              as HS (Input, Response, ServerPartT, Method (..), methodM,
                                              ok, seeOther, toResponse)
import           Text.Blaze.Html5            (ToMarkup, Html, toHtml, a, br, div, h3, h4,
                                              pre, table, td, toValue, tr, (!), input)
import qualified Text.Blaze.Html5    as H    (form, label)
import           Text.Blaze.Html5.Attributes as A (id, href, type_, name, value, checked, for, action, method, enctype)
import           Text.Reform                 (eitherForm, viewForm, (<++), CommonFormError, ErrorInputType,
                                                Form, FormError (..), FormInput)
import           Text.Reform.Blaze.String    (inputCheckboxes, label, textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT, RouteT)
import qualified Text.Reform.Generalized             as G
import           Web.Routes.Base
import           Web.Routes.Happstack                ()
import           Web.Routes.RouteT
import           Web.Routes.PathInfo
import           Web.Routes.TH
import  Data.Text                           (Text)
default (Integer, Double, Data.Text.Text)

data InputResult = InputResult EventNumber SignalAddress FormField InputData

data WebState n s = WebState {session      :: TVar s,
                              updateSession :: TVar s -> InputResult -> IO (),
                              evalFunc     :: forall a. (Show a) => n a -> Evaluate n s (),       -- evaluation function
                              errorHandler :: EventNumber -> String -> Evaluate n s ()}    -- error function
                              --getEvents :: TVar s -> IO [EventInfo n]}

type RoutedServer n s a = RouteT Command (StateT (WebState n s) (ServerPartT IO)) a

data Command =
    Main
  | DoInput   EventNumber SignalAddress FormField
  deriving (Show)


data ImpFormError = ImpFormError (CommonFormError [HS.Input])

instance FormError ImpFormError where
    type ErrorInputType ImpFormError = [HS.Input]
    commonFormError = ImpFormError

--instance ToMarkup ImpError where
--    toMarkup (ImpFormError e)       = fromString $ show e

type ImpForm a = Form (ServerPartT IO) [HS.Input] ImpFormError Html () a

instance PathInfo SignalAddressElem
instance PathInfo SignalAddress
instance PathInfo FormField
instance PathInfo (Int, String)
instance PathInfo [(Int, String)]
$(derivePathInfo ''Command)
