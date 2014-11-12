{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Nomyx.Web.Game.Details where

import Data.Maybe
import Data.String
import Data.List
import Data.Text (Text)
import Data.Time
import System.Locale
import Language.Nomyx
import Text.Blaze.Html5                    (Html, (!), p, table, thead, td, tr, h3, h4)
import Text.Blaze.Html5.Attributes as A    (style, class_)
import qualified Nomyx.Web.Help as Help
import Nomyx.Web.Common as NWC
import Nomyx.Core.Engine
default (Integer, Double, Data.Text.Text)


viewDetails :: PlayerNumber -> Game -> Html
viewDetails pn g = do
   p $ titleWithHelpIcon (h4 "Variables:") Help.variables
   viewVars   (_variables g)
   p $ titleWithHelpIcon (h4 "Events:") Help.events
   viewEvents g
   p $ h4 "Log:"
   viewLogs    (_logs g) pn

viewEvents :: Game -> Html
viewEvents g = table ! class_ "table" $ do
         thead $ do
            td ! class_ "td" $ "Event Number"
            td ! class_ "td" $ "By Rule"
            td ! class_ "td" $ "Event"
         mapM_ (viewEvent g) (sort $ _events g)

viewEvent :: Game -> EventInfo -> Html
viewEvent g ei@(EventInfo eventNumber ruleNumber _ _ status _) = if status == SActive then disp else disp ! style "background:gray;" where
   disp = tr $ do
      td ! class_ "td" $ fromString . show $ eventNumber
      td ! class_ "td" $ fromString . show $ ruleNumber
      td ! class_ "td" $ fromString . show $ getRemainingSignals ei g

viewVars :: [Var] -> Html
viewVars vs = table ! class_ "table" $ do
      thead $ do
         td ! class_ "td" $ "Rule number"
         td ! class_ "td" $ "Name"
         td ! class_ "td" $ "Value"
      mapM_ viewVar vs

viewVar :: Var -> Html
viewVar (Var vRuleNumber vName vData) = tr $ do
   td ! class_ "td" $ fromString . show $ vRuleNumber
   td ! class_ "td" $ fromString . show $ vName
   td ! class_ "td" $ fromString . show $ vData

viewLogs :: [Log] -> PlayerNumber -> Html
viewLogs log pn = do
   let ls = filter (\o -> (_lPlayerNumber o == Just pn) || (isNothing $ _lPlayerNumber o)) log
   table $ mapM_ viewLog (reverse ls)

viewLog :: Log -> Html
viewLog (Log _ t s) = tr $ do
   td $ fromString $ formatTime defaultTimeLocale "%Y/%m/%d_%H:%M" t
   td $ p $ fromString s
