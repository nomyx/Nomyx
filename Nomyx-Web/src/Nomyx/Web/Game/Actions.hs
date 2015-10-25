{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}

module Nomyx.Web.Game.Actions where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
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
import           Text.Blaze.Html5            (Html, a, br, div, h3, h4, pre,
                                              table, td, toValue, tr, (!))
import           Text.Blaze.Html5.Attributes as A (id)
import           Text.Reform                 (eitherForm, viewForm, (<++))
import           Text.Reform.Blaze.String    (inputCheckboxes, label, textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT, showURL)
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
   vior <- viewIORuleM pn (_rNumber r) g
   ok $ when (isJust vior) $ div ! A.id "IORule" $ do
      div ! A.id "IORuleTitle" $ h4 $ fromString $ "Rule #" ++ (show $ _rNumber r) ++ " \"" ++ (_rName $ _rRuleTemplate r) ++ "\": "
      fromJust vior

viewIORuleM :: PlayerNumber -> RuleNumber -> Game -> RoutedNomyxServer (Maybe Html)
viewIORuleM pn rn g = do
   vir <- viewInputsRule pn rn (_events g) g
   let vor = viewOutputsRule pn rn g
   return $ if isJust vir || isJust vor then Just $ do
      when (isJust vir) $ fromJust vir
      when (isJust vor) $ fromJust vor
   else Nothing

viewInputsRule :: PlayerNumber -> RuleNumber -> [EventInfo] -> Game -> RoutedNomyxServer (Maybe Html)
viewInputsRule pn rn ehs g = do
   let filtered = filter (\e -> _ruleNumber e == rn) ehs
   mis <- mapM (viewInput pn g) $ sort filtered
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

viewInput :: PlayerNumber -> Game -> EventInfo -> RoutedNomyxServer (Maybe Html)
viewInput me g ei@(EventInfo en _ _ _ SActive _) = do
   ds <- mapMaybeM (viewInput' me (_gameName g) en) (getRemainingSignals ei g)
   return $ if null ds
      then Nothing
      else Just $ sequence_ ds
viewInput _ _ _ = return Nothing

viewInput' :: PlayerNumber -> GameName -> EventNumber -> (SignalAddress, SomeSignal) -> RoutedNomyxServer (Maybe Html)
viewInput' me gn en (fa, ev@(SomeSignal (Input pn title _))) | me == pn = do
  lf  <- liftRouteT $ lift $ viewForm "user" $ inputForm ev
  link <- showURL (DoInput en fa (fromJust $ getFormField ev) gn)
  return $ Just $ tr $ td $ do
     fromString title
     fromString " "
     blazeForm lf link ! A.id "InputForm"
viewInput' _ _ _ _ = return Nothing

viewOutput :: Game -> Output -> Maybe Html
viewOutput g o = if (s /= "") then Just (pre $ fromString s >> br) else Nothing where
   s =  (evalOutput g o)

--- TODO: merge SomeSignal and FormField...
inputForm :: SomeSignal -> NomyxForm InputData
inputForm (SomeSignal (Input _ _ (Radio choices)))    = RadioData    <$> NWC.inputRadio' (zip [0..] (snd <$> choices)) (== 0) <++ label (" " :: String)
inputForm (SomeSignal (Input _ _ Text))               = TextData     <$> RB.inputText "" <++ label (" " :: String)
inputForm (SomeSignal (Input _ _ TextArea))           = TextAreaData <$> textarea 50 5  "" <++ label (" " :: String)
inputForm (SomeSignal (Input _ _ Button))             = pure ButtonData
inputForm (SomeSignal (Input _ _ (Checkbox choices))) = CheckboxData <$> inputCheckboxes (zip [0..] (snd <$> choices)) (const False) <++ label (" " :: String)
inputForm _ = error "Not an input form"


inputForm' :: FormField -> NomyxForm InputData
inputForm' (RadioField _ _ choices)    = RadioData    <$> NWC.inputRadio' choices (== 0) <++ label (" " :: String)
inputForm' (TextField _ _)             = TextData     <$> RB.inputText "" <++ label (" " :: String)
inputForm' (TextAreaField _ _)         = TextAreaData <$> textarea 50 5  "" <++ label (" " :: String)
inputForm' (ButtonField _ _)           = pure ButtonData
inputForm' (CheckboxField _ _ choices) = CheckboxData <$> inputCheckboxes choices (const False) <++ label (" " :: String)

-- | a form result has been sent
newInput :: EventNumber -> SignalAddress -> FormField -> GameName -> RoutedNomyxServer Response
newInput en fa ft gn = toResponse <$> do
   pn <- fromJust <$> getPlayerNumber
   link <- showURL (Menu Rules gn)
   methodM POST
   r <- liftRouteT $ lift $ eitherForm environment "user" (inputForm' ft)
   case r of
      (Right c) -> webCommand $ S.inputResult pn en fa ft c gn
      (Left _) ->  liftIO $ putStrLn "cannot retrieve form data"
   seeOther (link `appendAnchor` inputAnchor) "Redirecting..."
