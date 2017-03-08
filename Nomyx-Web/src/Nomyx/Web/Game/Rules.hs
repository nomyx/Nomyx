{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Nomyx.Web.Game.Rules where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.String
import           Data.Text                   (Text, unpack)
import           Nomyx.Language
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
   h3 "Constitution"
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
   vrm <- viewRuleMain pn g ri
   vrds <- mapM viewRuleDecl (_rModules ri) 
   ok $ div ! A.class_ "rule" ! A.id (toValue ("rule" ++ (show $ _rNumber ri))) $ do
      vrm
      sequence_ vrds

viewRuleMain :: PlayerNumber -> Game -> RuleInfo -> RoutedNomyxServer Html
viewRuleMain pn g ri@(RuleInfo rn proposedBy _ status assessBy mods (RuleTemplate name desc code author picture _ _))= do
  ios <- viewIORule pn g ri
  ok $ div ! A.class_ "ruleMain" $ do
      let pl = fromMaybe ("Player " ++ (show proposedBy)) (_playerName <$> (Profile.getPlayerInfo g proposedBy))
      let pic = fromMaybe "/static/pictures/democracy.png" picture 
      h2 $ fromString name
      img ! (A.src $ toValue $ pic)
      h3 $ fromString $ desc
      h2 $ fromString $ "proposed by " ++ (if proposedBy == 0 then "System" else pl)
      let assessedBy = case assessBy of
           Nothing -> fromString "not assessed"
           Just 0  -> fromString "the system"
           Just a  -> H.a (fromString $ "rule " ++ show a) ! (href $ toValue $ "?ruleNumber=" ++ (show a))
      case status of
          Active -> (fromString "This rule was activated by ") >> assessedBy >> (fromString ".") ! A.id "assessedBy"
          Reject -> (fromString "This rule was deleted by ") >> assessedBy >> (fromString ".") ! A.id "assessedBy"
          Pending -> return ()
      ios
      viewRuleCode code
      br
      sequence_ $ map (declLink rn) (map _modPath mods)


viewRuleDecl :: ModuleInfo -> RoutedNomyxServer Html
viewRuleDecl (ModuleInfo path cont) = do
   ok $ div ! A.class_ "ruleDecl" ! A.id (toValue ("ruleDecl" ++ (idEncode path))) $ do
      displayCode $ unpack cont
       
declLink :: RuleNumber -> FilePath -> Html
declLink rn modPath = do
   H.a (fromString modPath) ! (href $ toValue $ "?ruleNumber=" ++ (show rn) ++ "&decl=" ++ (idEncode modPath))
   br

