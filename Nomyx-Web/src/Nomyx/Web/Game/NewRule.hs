{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Nomyx.Web.Game.NewRule where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.String
import           Data.Text                   (Text)
import           Happstack.Server            (Method (..), Response, methodM,
                                              ok, seeOther, toResponse)
import           Language.Nomyx
import           Nomyx.Core.Engine
import           Nomyx.Core.Session          as S
import           Nomyx.Core.Types            as T
import           Nomyx.Web.Common            as NWC
import qualified Nomyx.Web.Help              as Help
import           Nomyx.Web.Types
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            as H (Html, a, div, h2, h3, img,
                                                   pre, toValue, (!), li, ul)
import           Text.Blaze.Html5.Attributes as A (class_, disabled, id,
                                                   placeholder, src, href)
import           Text.Reform                 (eitherForm, viewForm, (++>),
                                              (<++))
import           Text.Reform.Blaze.Common    (setAttr)
import           Text.Reform.Blaze.String    (inputSubmit, label, textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT, showURL)
default (Integer, Double, Data.Text.Text)

viewLibrary :: [RuleTemplate] -> RoutedNomyxServer Html
viewLibrary lib = do
  vrs <- viewRules lib
  ok $ do
    div ! class_ "ruleList" $ ul $ viewRuleNames lib
    div ! class_ "rules" $ vrs

viewRuleNames :: [RuleTemplate] -> Html
viewRuleNames nrs = mapM_  viewRuleName nrs

viewRuleName :: RuleTemplate -> Html
viewRuleName rd = do
  let name = fromString $ _rName $ rd
  li $ H.a name ! A.class_ "ruleName" ! (A.href $ toValue $ "#rule" ++ (show $ _rName rd))

viewRules :: [RuleTemplate] -> RoutedNomyxServer Html
viewRules rds = do
  vrs <- mapM viewRule rds
  ok $ sequence_ vrs

viewRule :: RuleTemplate -> RoutedNomyxServer Html
viewRule rd = do
  ok $ div ! A.class_ "rule" ! A.id (toValue ("rule" ++ (show $ _rName rd))) $ do
   let pic = fromMaybe "/static/pictures/democracy.png" (_rPicture rd)
   h2 $ fromString $ _rName rd
   img ! (A.src $ toValue $ pic)
   h3 $ fromString $ _rDescription rd
   h2 $ fromString $ "authored by " ++ (_rAuthor rd)
   viewRuleFunc rd

newRuleForm :: Maybe RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String, Maybe String)
newRuleForm (Just sr) isGameAdmin = newRuleForm' sr isGameAdmin
newRuleForm Nothing isGameAdmin = newRuleForm' (RuleTemplate "" "" "" "" Nothing []) isGameAdmin

newRuleForm' :: RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String, Maybe String)
newRuleForm' (RuleTemplate name desc code "" Nothing []) isGameAdmin =
   (,,) <$> submitRuleForm name desc code
        <*> inputSubmit "Check"
        <*> if isGameAdmin then inputSubmit "Admin submit" else pure Nothing

submitRuleForm :: String -> String -> String -> NomyxForm RuleTemplate
submitRuleForm name desc code =
   RuleTemplate <$> label "Name: " ++> RB.inputText name `setAttr` class_ "ruleName"
               <*> (label "      Short description: " ++> (RB.inputText desc `setAttr` class_ "ruleDescr") <++ RB.br)
               <*> label "      Code: " ++> textarea 80 15 code `setAttr` class_ "ruleCode" `setAttr` placeholder "Enter here your rule"
               <*> pure "" <*> pure Nothing <*> pure []

viewRuleForm :: Maybe LastRule -> Bool -> Bool -> GameName -> RoutedNomyxServer Html
viewRuleForm mlr inGame isGameAdmin gn = do
   link <- showURL (NewRule gn)
   lf  <- liftRouteT $ lift $ viewForm "user" (newRuleForm (fst <$> mlr) isGameAdmin)
   ok $ do
      a "" ! A.id (toValue ruleFormAnchor)
      titleWithHelpIcon (h3 "Propose a new rule:") Help.code
      if inGame then do
         blazeForm lf link
         let msg = snd <$> mlr
         when (isJust msg) $ pre $ fromString $ fromJust msg
      else lf ! disabled ""

newRule :: GameName -> RoutedNomyxServer Response
newRule gn = toResponse <$> do
   methodM POST
   s <- getSession
   let gi = getGameByName gn s
   admin <- isGameAdmin (fromJust gi)
   r <- liftRouteT $ lift $ eitherForm environment "user" (newRuleForm Nothing admin)
   link <- showURL MainPage
   pn <- fromJust <$> getPlayerNumber
   case r of
      Right (sr, Nothing, Nothing) -> webCommand $ submitRule sr pn gn (_sh s)
      Right (sr, Just _, Nothing)  -> webCommand $ checkRule sr pn (_sh s)
      Right (sr, Nothing, Just _)  -> webCommand $ adminSubmitRule sr pn gn (_sh s)
      Right (_,  Just _, Just _)   -> error "Impossible new rule form result"
      (Left _) -> liftIO $ putStrLn "cannot retrieve form data"
   seeOther (link `appendAnchor` ruleFormAnchor) $ "Redirecting..."

viewRuleFunc :: RuleTemplate -> Html
viewRuleFunc rd = do
  let code = lines $ _rRuleCode rd
  let codeCutLines = 7
  --let ref = "openModalCode" ++ (show $ _rNumber ri) ++ "game" ++ gn

  --div ! A.id "showCodeLink" $ a ! (href $ toValue $ "#" ++ ref)  $ "show more..." >> br
  div ! A.id "codeDiv" $ displayCode $ unlines $ take codeCutLines code
  div $ when (length code >= codeCutLines) $ fromString "(...)"
  -- div ! A.id (toValue ref) ! class_ "modalDialog" $ do
  --    div $ do
  --       p "Code of the rule:"
  --       a ! href "#close" ! title "Close" ! class_ "close" $ "X"
  --       div ! A.id "modalCode" $ do
  --          displayCode $ unlines code
  --          br
