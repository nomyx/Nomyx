{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances   #-}

module Imprevu.Happstack.Types where

import Control.Concurrent.STM
import Control.Lens
import Happstack.Server                  (ServerPartT, FromReqURI(..))
import qualified Happstack.Server     as HS (Input)
import Imprevu
import Imprevu.Evaluation
import Text.Blaze.Html5                  (Html)
import Text.Reform                       (CommonFormError, ErrorInputType, Form, FormError (..))
import Safe

type UpdateSession s = TVar s -> Input -> InputData -> EventNumber -> IO () -- update the session after an input is submitted

type ImpForm a = Form (ServerPartT IO) [HS.Input] ImpFormError Html () a

data ImpFormError = ImpFormError (CommonFormError [HS.Input])

instance FormError ImpFormError where
    type ErrorInputType ImpFormError = [HS.Input]
    commonFormError = ImpFormError

instance FromReqURI Input where
    fromReqURI = readMay

instance FromReqURI InputData where
    fromReqURI = readMay
