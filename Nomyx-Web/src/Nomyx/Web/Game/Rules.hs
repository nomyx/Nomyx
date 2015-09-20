{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

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
import qualified Nomyx.Web.Help              as Help
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            (Html, a, br, div, h3, h4, p,
                                              table, td, thead, toHtml, toValue,
                                              tr, (!))
import           Text.Blaze.Html5.Attributes as A (class_, href, id, title)
default (Integer, Double, Data.Text.Text)

viewAllRules :: Game -> Html
viewAllRules g = do
   titleWithHelpIcon (h3 "Rules") Help.rules
   viewRules (activeRules g)   "Active rules"     True g >> br
   viewRules (pendingRules g)  "Pending rules"    True g >> br
   viewRules (rejectedRules g) "Suppressed rules" False g >> br

viewRules :: [RuleInfo] -> String -> Bool -> Game -> Html
viewRules nrs title visible g = showHideTitle ((_gameName g) ++ title) visible (null nrs) (h4 $ toHtml (title ++ ":") ) $ table ! class_ "table" $ do
   thead $ do
      td ! class_ "td" $ "#"
      td ! class_ "td" $ "Name"
      td ! class_ "td" $ "Description"
      td ! class_ "td" $ "Code of the rule"
   forM_ nrs (viewRule g)

viewRule :: Game -> RuleInfo -> Html
viewRule g ri = tr $ do
   let pl = fromMaybe ("Player " ++ (show $ _rProposedBy ri)) (_playerName <$> (Profile.getPlayerInfo g $ _rProposedBy ri))
   td ! class_ "td" $ p (fromString . show $ _rNumber ri) ! A.id "ruleNumber"
   td ! class_ "td" $ do
      div ! A.id "ruleName" $ (fromString $ _rName $ _rRuleDetails ri)
      br
      div ! A.id "proposedBy" $ (fromString $ "by "  ++ (if _rProposedBy ri == 0 then "System" else pl))
   td ! class_ "td" $ fromString $ _rDescription $ _rRuleDetails ri
   td ! class_ "td" $ viewRuleFunc ri (_gameName g)

viewRuleFunc :: RuleInfo -> GameName -> Html
viewRuleFunc ri gn = do
   let code = lines $ _rRuleCode $ _rRuleDetails ri
   let codeCutLines = 7
   let ref = "openModalCode" ++ (show $ _rNumber ri) ++ "game" ++ gn
   let assessedBy = case _rAssessedBy ri of
        Nothing -> "not assessed"
        Just 0  -> "the system"
        Just a  -> "rule " ++ show a
   div ! A.id "showCodeLink" $ a ! (href $ toValue $ "#" ++ ref)  $ "show more..." >> br
   div ! A.id "codeDiv" $ displayCode $ unlines $ take codeCutLines code
   div $ when (length code >= codeCutLines) $ fromString "(...)"
   div ! A.id (toValue ref) ! class_ "modalDialog" $ do
      div $ do
         p "Code of the rule:"
         a ! href "#close" ! title "Close" ! class_ "close" $ "X"
         div ! A.id "modalCode" $ do
            displayCode $ unlines code
            br
            case _rStatus ri of
               Active -> (fromString $ "This rule was activated by " ++ assessedBy ++ ".") ! A.id "assessedBy"
               Reject -> (fromString $ "This rule was deleted by " ++ assessedBy ++ ".") ! A.id "assessedBy"
               Pending -> return ()
