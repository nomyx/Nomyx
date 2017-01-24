{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Nomyx.Web.Game.Modules where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.Function (on)
import           Data.String
import           Data.List (sortBy, groupBy)
import           Data.Ord (comparing)
import           Data.Text                   (Text, pack, unpack)
import           Data.Text.Encoding
import           Happstack.Server            (Method (..), Response, methodM,
                                              ok, seeOther, toResponse)
import           Nomyx.Language
import           Nomyx.Core.Engine
import           Nomyx.Core.Session          as S
import           Nomyx.Core.Types            as T
import           Nomyx.Core.Utils
import           Nomyx.Web.Common            as NWC
import qualified Nomyx.Web.Help              as Help
import           Nomyx.Web.Types
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            as H (Html, a, div, h2, h3, h4,
                                                   img, input, label, li, pre,
                                                   toValue, ul, (!), p)
import           Text.Blaze.Html5.Attributes as A (class_, disabled, for, href,
                                                   id, placeholder, src, type_,
                                                   value)
import           Text.Reform                 (eitherForm, viewForm, (++>),
                                              (<++))
import           Text.Reform.Blaze.Common    (setAttr)
import           Text.Reform.Blaze.String    (inputHidden, inputSubmit, label,
                                              textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT)
import           Happstack.Server            (ContentType)
import           Safe
import           Network.HTTP.Base                  (urlEncode)
default (Integer, Double, Data.Text.Text)


-- * Library display

viewModules :: Library -> Maybe LastRule -> GameName -> Bool -> RoutedNomyxServer Html
viewModules (Library rts ms) mlr gn isGameAdmin = do
  ms <- mapM (viewPaneModule gn mlr isGameAdmin) ms
  ok $ do
    div ! class_ "modules" $ sequence_ ms

viewPaneModule :: GameName -> Maybe LastRule -> Bool -> ModuleInfo -> RoutedNomyxServer Html
viewPaneModule gn mlr isGameAdmin modi = do
  view <- viewModule gn modi isGameAdmin
  ok $ div ! A.class_ "rule" ! A.id (toValue $ idEncode $ _modPath modi) $ do
    view

-- ** Module view

viewModule :: GameName -> ModuleInfo -> Bool -> RoutedNomyxServer Html
viewModule gn (ModuleInfo path mod) isGameAdmin = do
  ok $ div $ do
    div ! A.id "codeDiv" $ displayCode $ unpack mod
    
