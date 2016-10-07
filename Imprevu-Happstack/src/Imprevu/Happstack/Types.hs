{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances   #-}

module Imprevu.Happstack.Types where

import Control.Concurrent.STM
import Control.Lens
import Happstack.Server            as HS (Input, ServerPartT, FromReqURI(..))
import Imprevu.Event
import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.InputEval
import Text.Blaze.Html5                  (Html)
import Text.Reform                       (CommonFormError, ErrorInputType, Form, FormError (..))
import Safe

data InputResult = InputResult EventNumber SignalAddress InputView InputDataView

data WebStateN n s = WebState {_webState     :: TVar s,
                               updateSession :: TVar s -> InputResult -> IO (),
                               evalFunc      :: forall a. n a -> EvaluateN n s a,       -- evaluation function
                               errorHandler  :: EventNumber -> String -> EvaluateN n s ()}    -- error function

type ImpForm a = Form (ServerPartT IO) [HS.Input] ImpFormError Html () a

data ImpFormError = ImpFormError (CommonFormError [HS.Input])

instance FormError ImpFormError where
    type ErrorInputType ImpFormError = [HS.Input]
    commonFormError = ImpFormError

instance FromReqURI SignalAddress where
    fromReqURI = readMay

instance FromReqURI InputView where
    fromReqURI = readMay

makeLenses ''WebStateN
