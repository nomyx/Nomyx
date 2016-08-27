{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances   #-}

module Imprevu.Happstack.Types where

import Control.Concurrent.STM
import Control.Monad.State
import Happstack.Server            as HS (Input, ServerPartT)
import Imprevu.Internal.Event
import Imprevu.Internal.EventEval
import Imprevu.Internal.InputEval
import Text.Blaze.Html5                  (Html)
import Text.Reform                       (CommonFormError, ErrorInputType, Form, FormError (..))
import Web.Routes.Happstack              ()
import Web.Routes.RouteT
import Web.Routes.PathInfo
import Web.Routes.TH

data InputResult = InputResult EventNumber SignalAddress InputView InputDataView

data WebState n s = WebState {session      :: TVar s,
                              updateSession :: TVar s -> InputResult -> IO (),
                              evalFunc     :: forall a. (Show a) => n a -> Evaluate n s (),       -- evaluation function
                              errorHandler :: EventNumber -> String -> Evaluate n s ()}    -- error function
                              --getEvents :: TVar s -> IO [EventInfo n]}

type RoutedServer n s a = RouteT Command (StateT (WebState n s) (ServerPartT IO)) a

data Command =
    Main
  | DoInput   EventNumber SignalAddress InputView
    deriving (Show)


data ImpFormError = ImpFormError (CommonFormError [HS.Input])

instance FormError ImpFormError where
    type ErrorInputType ImpFormError = [HS.Input]
    commonFormError = ImpFormError

type ImpForm a = Form (ServerPartT IO) [HS.Input] ImpFormError Html () a

instance PathInfo SignalAddressElem
instance PathInfo SignalAddress
instance PathInfo InputView
instance PathInfo (Int, String)
instance PathInfo [(Int, String)]
$(derivePathInfo ''Command)
