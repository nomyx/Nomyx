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
import           Nomyx.Web.Game.IOs
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
  let name = fromString $ (show $ _rNumber ri) ++ " " ++ (_rName $ _rRuleDetails ri)
  li $ H.a name ! A.class_ "ruleName" ! (href $ toValue $ "#rule" ++ (show $ _rNumber ri))

viewRules :: PlayerNumber -> Game -> [RuleInfo] -> RoutedNomyxServer Html
viewRules pn g nrs = do
  (vrs :: [Html]) <- mapM (viewRule pn g) nrs
  ok $ sequence_ vrs

viewRule :: PlayerNumber -> Game -> RuleInfo -> RoutedNomyxServer Html
viewRule pn g ri = do
  ios <- viewIORule pn g ri
  ok $ div ! A.class_ "rule" ! A.id (toValue ("rule" ++ (show $ _rNumber ri))) $ do
   let pl = fromMaybe ("Player " ++ (show $ _rProposedBy ri)) (_playerName <$> (Profile.getPlayerInfo g $ _rProposedBy ri))
   let pic = fromMaybe "/static/pictures/democracy.png" (_rPicture $ _rRuleDetails ri)
   h2 $ fromString $ _rName $ _rRuleDetails ri
   img ! (A.src $ toValue $ pic)
   h3 $ fromString $ _rDescription $ _rRuleDetails ri
   h2 $ fromString $ "proposed by " ++ (if _rProposedBy ri == 0 then "System" else pl)
   ios

--   td ! class_ "td" $ viewRuleFunc ri (_gameName g)

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
