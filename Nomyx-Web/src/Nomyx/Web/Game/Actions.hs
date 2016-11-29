{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}

module Nomyx.Web.Game.Actions where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Lens hiding (pre)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text, pack)
import           Happstack.Server            (Method (..), Response, methodM,
                                              ok, seeOther, toResponse)
import           Language.Nomyx
import           Nomyx.Core.Engine
import           Nomyx.Core.Mail
import           Nomyx.Core.Session          as S
import           Nomyx.Web.Common            as NWC
import qualified Nomyx.Web.Help              as Help
import           Nomyx.Web.Types
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            (Html, a, br, div, h3, h4,
                                              pre, table, td, toValue, tr, (!))
import           Text.Blaze.Html5.Attributes as A (id, href)
import           Text.Reform                 (eitherForm, viewForm, (<++))
import           Text.Reform.Blaze.String    (inputCheckboxes, label, textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT)
import           Imprevu.Happstack.Forms     (viewInput, newInput)
import           Imprevu.Happstack.Types
import           Imprevu.Evaluation.InputEval

default (Integer, Double, Data.Text.Text)


viewIOs :: PlayerNumber -> Game -> RoutedNomyxServer Html
viewIOs pn g = do
   vios <- mapM (viewIORule pn g) (sort $ _rules g)
   ok $ do
      titleWithHelpIcon (h3 "Inputs/Ouputs") Help.inputsOutputs
      a "" ! A.id (toValue inputAnchor)
      mconcat vios

viewIORule :: PlayerNumber -> Game -> RuleInfo -> RoutedNomyxServer Html
viewIORule pn g r = do
   let ruleLink = showRelURLParams (Menu Rules $ _gameName g) [("ruleNumber", Just $ pack $ show $ _rNumber r)]
   vior <- viewIORuleM pn (_rNumber r) g
   ok $ when (isJust vior) $ div ! A.id "IORule" $ do
      div ! A.id "IORuleTitle" $ h4 $ a (fromString $ "Rule " ++ (show $ _rNumber r) ++ " \"" ++ (_rName $ _rRuleTemplate r) ++ "\": ") ! (A.href $ toValue ruleLink)
      fromJust vior

viewIORuleM :: PlayerNumber -> RuleNumber -> Game -> RoutedNomyxServer (Maybe Html)
viewIORuleM pn rn g = do
   vir <- viewInputsRule pn rn (_events g) g
   let vor = viewOutputsRule pn rn g
   return $ if isJust vir || isJust vor then Just $ do
      when (isJust vir) $ fromJust vir
      when (isJust vor) $ fromJust vor
   else Nothing

viewInputsRule :: PlayerNumber -> RuleNumber -> [RuleEventInfo] -> Game -> RoutedNomyxServer (Maybe Html)
viewInputsRule pn rn ehs g = do
   let filtered = filter (\e -> _erRuleNumber e == rn) ehs
   let link iv = showRelURL (DoInput iv (_gameName g))
   ws <- use webSession
   mis <- liftRouteT $ lift $ mapM (viewInput pn ws link) $ (map _erEventInfo $ sort filtered)
   let is = catMaybes mis
   case is of
      [] -> return Nothing
      i -> return $ Just $ table $ mconcat i

viewOutputsRule :: PlayerNumber -> RuleNumber -> Game -> Maybe Html
viewOutputsRule pn rn g = do
   let filtered = filter (\o -> _oRuleNumber o == rn && _oStatus o == SActive) (_outputs g)
   let myos = filter (isPn pn) (sort filtered)
   case myos of
      [] -> Nothing
      os -> Just $ sequence_ $ mapMaybe (viewOutput g) os

isPn pn (Output _ _ (Just mypn) _ SActive) = mypn == pn
isPn _  (Output _ _ Nothing _ SActive) = True
isPn _ _ = False

viewOutput :: Game -> Output -> Maybe Html
viewOutput g o = if (s /= "") then Just (pre $ fromString s >> br) else Nothing where
   s =  (evalOutput g o)

---- | a form result has been sent
newInput' :: InputS -> GameName -> RoutedNomyxServer Response
newInput' is gn = do
  ws <- use webSession
  let link = showRelURL $ Menu Actions gn
  liftRouteT $ lift $ newInput is ws link
