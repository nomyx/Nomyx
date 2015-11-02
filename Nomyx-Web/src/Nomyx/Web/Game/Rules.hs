{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Nomyx.Web.Game.Rules where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.String
import           Data.Text                   (Text)
import           Language.Nomyx
import           Nomyx.Core.Engine
import           Nomyx.Core.Profile          as Profile
import           Nomyx.Web.Common            as NWC
import           Nomyx.Web.Game.Actions
import qualified Nomyx.Web.Help              as Help
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            as H (Html, a, br, div, h2, h3, h4,
                                                   img, li, p, table, td, thead,
                                                   toHtml, toValue, tr, ul, (!))
import           Text.Blaze.Html5.Attributes as A (class_, href, id, src, title)
import           Happstack.Server            (ok)
import           Nomyx.Web.Types
default (Integer, Double, Data.Text.Text)

viewAllRules :: PlayerNumber -> Game -> RoutedNomyxServer Html
viewAllRules pn g = do
  vrs <- viewRules pn g (_rules g)
  ok $ do
  div ! class_ "ruleList" $ do
   ul $ do
     li "Active rules"
     ul $ viewRuleNames (activeRules g)
     li "Pending rules"
     ul $ viewRuleNames (pendingRules g)
     li "Suppressed rules"
     ul $ viewRuleNames (rejectedRules g)
  div ! class_ "rules" $ vrs

viewRuleNames :: [RuleInfo] -> Html
viewRuleNames nrs = mapM_  viewRuleName nrs

viewRuleName :: RuleInfo -> Html
viewRuleName ri = do
  let name = fromString $ (show $ _rNumber ri) ++ " " ++ (_rName $ _rRuleTemplate ri)
  li $ H.a name ! A.class_ "ruleName" ! (href $ toValue $ "?ruleNumber=" ++ (show $ _rNumber ri))

viewRules :: PlayerNumber -> Game -> [RuleInfo] -> RoutedNomyxServer Html
viewRules pn g nrs = do
  (vrs :: [Html]) <- mapM (viewRule pn g) nrs
  ok $ sequence_ vrs

viewRule :: PlayerNumber -> Game -> RuleInfo -> RoutedNomyxServer Html
viewRule pn g ri = do
  ios <- viewIORule pn g ri
  let assessedBy = case _rAssessedBy ri of
       Nothing -> "not assessed"
       Just 0  -> "the system"
       Just a  -> "rule " ++ show a
  ok $ div ! A.class_ "rule" ! A.id (toValue ("rule" ++ (show $ _rNumber ri))) $ do
   let pl = fromMaybe ("Player " ++ (show $ _rProposedBy ri)) (_playerName <$> (Profile.getPlayerInfo g $ _rProposedBy ri))
   let pic = fromMaybe "/static/pictures/democracy.png" (_rPicture $ _rRuleTemplate ri)
   h2 $ fromString $ _rName $ _rRuleTemplate ri
   img ! (A.src $ toValue $ pic)
   h3 $ fromString $ _rDescription $ _rRuleTemplate ri
   h2 $ fromString $ "proposed by " ++ (if _rProposedBy ri == 0 then "System" else pl)
   case _rStatus ri of
      Active -> (fromString $ "This rule was activated by " ++ assessedBy ++ ".") ! A.id "assessedBy"
      Reject -> (fromString $ "This rule was deleted by " ++ assessedBy ++ ".") ! A.id "assessedBy"
      Pending -> return ()
   ios

--   td ! class_ "td" $ viewRuleFunc ri (_gameName g)
