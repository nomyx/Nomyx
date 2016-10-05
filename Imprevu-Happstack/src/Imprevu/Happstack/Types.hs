{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances   #-}

module Imprevu.Happstack.Types where

import Control.Concurrent.STM
import Control.Monad.State
import Control.Lens
import Happstack.Server            as HS (Input, ServerPartT)
import Imprevu.Event
import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.InputEval
import Text.Blaze.Html5                  (Html)
import Text.Reform                       (CommonFormError, ErrorInputType, Form, FormError (..))
import Web.Routes.Happstack              ()
import Web.Routes.RouteT
import Web.Routes.PathInfo
import Web.Routes.TH

data InputResult = InputResult EventNumber SignalAddress InputView InputDataView

data WebStateN n s = WebState {_webState     :: TVar s,
                               updateSession :: TVar s -> InputResult -> IO (),
                               evalFunc      :: forall a. n a -> EvaluateN n s a,       -- evaluation function
                               errorHandler  :: EventNumber -> String -> EvaluateN n s ()}    -- error function

type RoutedServer n s a = RouteT Command (StateT (WebStateN n s) (ServerPartT IO)) a

data Command =
    Main
  | DoInput   EventNumber SignalAddress InputView
    deriving (Show)


type ImpForm a = Form (ServerPartT IO) [HS.Input] ImpFormError Html () a

data ImpFormError = ImpFormError (CommonFormError [HS.Input])

instance FormError ImpFormError where
    type ErrorInputType ImpFormError = [HS.Input]
    commonFormError = ImpFormError


instance PathInfo SignalAddressElem
instance PathInfo SignalAddress
instance PathInfo InputView
instance PathInfo (Int, String)
instance PathInfo [(Int, String)]
$(derivePathInfo ''Command)

makeLenses ''WebStateN
