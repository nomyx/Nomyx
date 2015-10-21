{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Nomyx.Web.Game.Templates where

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
import           Text.Blaze.Html5            as H (Html, a, div, h2, h3, h4, img,
                                                   input, li, pre, toValue, ul,
                                                   (!), label)
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
import           Web.Routes.RouteT           (liftRouteT, showURL)
default (Integer, Double, Data.Text.Text)


-- * Templates display

viewRuleTemplates :: [RuleTemplate] -> GameName -> RoutedNomyxServer Html
viewRuleTemplates rts gn = do
  vrs <- mapM (viewRuleTemplate gn) rts
  ok $ do
    div ! class_ "ruleList" $ ul $ viewRuleTemplateNames rts
    div ! class_ "rules" $ do

      sequence_ vrs

viewRuleTemplateNames :: [RuleTemplate] -> Html
viewRuleTemplateNames nrs = mapM_  viewRuleTemplateName nrs

viewRuleTemplateName :: RuleTemplate -> Html
viewRuleTemplateName rd = do
  let name = fromString $ _rName $ rd
  li $ H.a name ! A.class_ "ruleName" ! (A.href $ toValue $ "#rule" ++ (show $ _rName rd))

viewRuleTemplate :: GameName -> RuleTemplate -> RoutedNomyxServer Html
viewRuleTemplate gn rt = do
  link <- showURL (SubmitRule gn)
  lf  <- liftRouteT $ lift $ viewForm "user" (hiddenSubmitRuleTemplatForm (Just rt))
  vrte <- viewRuleTemplateEdit (Just (rt, ""))
  ok $ do
    div ! A.class_ "rule" ! A.id (toValue ("rule" ++ (show $ _rName rt))) $ do
      H.label ! A.for "isruleedit" $ "edit"
      H.input ! A.type_ "checkbox" ! A.id "isruleedit"
      div ! A.id "viewrule" $ do
        let pic = fromMaybe "/static/pictures/democracy.png" (_rPicture rt)
        h2 $ fromString $ _rName rt
        img ! (A.src $ toValue $ pic)
        h3 $ fromString $ _rDescription rt
        h2 $ fromString $ "authored by " ++ (_rAuthor rt)
        viewRuleFunc rt
        blazeForm lf link
      div ! A.id "editRule" $ vrte



hiddenSubmitRuleTemplatForm :: (Maybe RuleTemplate) -> NomyxForm String
hiddenSubmitRuleTemplatForm rt = inputHidden (show rt)

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


-- * Templates submit

-- | Submit a template to a given game
submitRuleTemplatePost :: GameName -> RoutedNomyxServer Response
submitRuleTemplatePost gn = toResponse <$> do
   methodM POST
   s <- getSession
   let gi = getGameByName gn s
   admin <- isGameAdmin (fromJust gi)
   r <- liftRouteT $ lift $ eitherForm environment "user" (hiddenSubmitRuleTemplatForm Nothing)
   link <- showURL MainPage
   pn <- fromJust <$> getPlayerNumber
   case r of
      Right rt -> webCommand $ submitRule (fromJust $ read rt) pn gn (_sh s)
    --  Right (rt, Just _, Nothing)  -> webCommand $ checkRule (read rt) pn (_sh s)
  --    Right (rt, Nothing, Just _)  -> webCommand $ adminSubmitRule (read rt) pn gn (_sh s)
  --    Right (_,  Just _, Just _)   -> error "Impossible new rule form result"
      (Left _) -> liftIO $ putStrLn "cannot retrieve form data"
   seeOther (link `appendAnchor` ruleFormAnchor) $ "Redirecting..."




-- * Template edit

-- Edit a template
viewRuleTemplateEdit :: Maybe LastRule -> RoutedNomyxServer Html
viewRuleTemplateEdit mlr = do
  link <- showURL NewRuleTemplate
  lf  <- liftRouteT $ lift $ viewForm "user" (newRuleTemplateForm (fst <$> mlr) True)
  ok $ div $ do
  --  let pic = fromMaybe "/static/pictures/democracy.png" (_rPicture rt)
  --  h2 $ fromString $ _rName rt
  --  img ! (A.src $ toValue $ pic)
  --  h3 $ fromString $ _rDescription rt
  --  h2 $ fromString $ "authored by " ++ (_rAuthor rt)
  --  viewRuleFunc rt
   blazeForm lf link

newRuleTemplateForm :: Maybe RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String)
newRuleTemplateForm (Just sr) isGameAdmin = newRuleTemplateForm' sr isGameAdmin
newRuleTemplateForm Nothing isGameAdmin = newRuleTemplateForm' (RuleTemplate "" "" "" "" Nothing []) isGameAdmin

newRuleTemplateForm' :: RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String)
newRuleTemplateForm' rt isGameAdmin =
  (,) <$> newRuleTemplateForm'' rt
       <*> inputSubmit "Check"
       -- <*> if isGameAdmin then inputSubmit "Admin submit" else pure Nothing

newRuleTemplateForm'' :: RuleTemplate -> NomyxForm RuleTemplate
newRuleTemplateForm'' (RuleTemplate name desc code aut pic cat) =
  RuleTemplate <$> RB.label "Name: " ++> RB.inputText name `setAttr` class_ "ruleName"
              <*> (RB.label "      Short description: " ++> (RB.inputText desc `setAttr` class_ "ruleDescr") <++ RB.br)
              <*> RB.label "      Code: " ++> textarea 80 15 code `setAttr` class_ "ruleCode" `setAttr` placeholder "Enter here your rule"
              <*> pure ""
              <*> pure Nothing
              <*> pure []

newRuleTemplate :: RoutedNomyxServer Response
newRuleTemplate = toResponse <$> do
  methodM POST
  s <- getSession
  r <- liftRouteT $ lift $ eitherForm environment "user" (newRuleTemplateForm Nothing False)
  link <- showURL MainPage
  pn <- fromJust <$> getPlayerNumber
  case r of
     Right (rt, Nothing) -> webCommand $ S.newRuleTemplate rt pn (_sh s)
     Right (rt, Just _)  -> webCommand $ S.checkRule rt pn (_sh s)
 --    Right (rt, Nothing, Just _)  -> webCommand $ adminSubmitRule (read rt) pn gn (_sh s)
 --    Right (_,  Just _, Just _)   -> error "Impossible new rule form result"
     (Left _) -> liftIO $ putStrLn "cannot retrieve form data"
  seeOther (link `appendAnchor` ruleFormAnchor) $ "Redirecting..."
