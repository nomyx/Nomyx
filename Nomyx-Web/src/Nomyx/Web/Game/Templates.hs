{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
--{-# LANGUAGE ApplicativeDo  #-}

module Nomyx.Web.Game.Templates where

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

viewLibrary :: Library -> Maybe LastRule -> GameName -> Bool -> RoutedNomyxServer Html
viewLibrary (Library rts ms) mlr gn isGameAdmin = do
  vrs <- mapM (viewPaneRuleTemplate gn mlr isGameAdmin) rts
  ok $ do
    div ! class_ "ruleList" $ viewRuleTemplateCats rts mlr
    div ! class_ "rules" $ sequence_ vrs

-- * left menu display

viewRuleTemplateCats :: [RuleTemplate] -> Maybe LastRule -> Html
viewRuleTemplateCats rts mlr = do
  let cat = (headDef "Not category" . _rCategory)
  let rts' = groupBy ((==) `on` cat) $ sortBy (comparing cat) rts
  --let allRules = rts' ++ [(maybe (RuleTemplate "New Rule" "" "" "" Nothing [] []) fst mlr)]
  h2 "Library of rules"
  ul $ mapM_  viewRuleTemplateCat rts'

viewRuleTemplateCat :: [RuleTemplate] -> Html
viewRuleTemplateCat rts = li $ do
   fromString $ headDef "No category" $ _rCategory $ head rts
   ul $ mapM_  viewRuleTemplateName rts

viewRuleTemplateName :: RuleTemplate -> Html
viewRuleTemplateName rt = li $ H.a (fromString $ _rName rt) ! A.class_ "ruleName" ! (A.href $ toValue $ "?ruleName=" ++ (idEncode $ _rName rt))


-- * main tab display

viewPaneRuleTemplate :: GameName -> Maybe LastRule -> Bool -> RuleTemplate -> RoutedNomyxServer Html
viewPaneRuleTemplate gn mlr isGameAdmin rt = do
  let toEdit = case mlr of
       Nothing -> (rt, "")
       Just lr -> if ((_rName $ fst lr) == (_rName rt)) then lr else (rt, "")
  com <- templateCommands gn rt
  view <- viewRuleTemplate gn toEdit isGameAdmin
  edit <- viewRuleTemplateEdit toEdit gn
  ok $ div ! A.class_ "rule" ! A.id (toValue $ idEncode $ _rName rt) $ do
    com
    view
    edit

-- ** Template commands

templateCommands :: GameName -> RuleTemplate -> RoutedNomyxServer Html
templateCommands gn rt = do
  let delLink = showRelURL (DelRuleTemplate gn (_rName rt))
  let idrt = idEncode $ _rName rt
  ok $ div ! A.class_ "commandrule" $ do
    p $ H.a "view"   ! (A.href $ toValue $ "?ruleName=" ++ idrt)
    p $ H.a "edit"   ! (A.href $ toValue $ "?ruleName=" ++ idrt ++ "&edit")
    p $ H.a "delete" ! (A.href $ toValue delLink)


delRuleTemplate :: GameName -> RuleName -> RoutedNomyxServer Response
delRuleTemplate gn rn = do
  pn <- fromJust <$> getPlayerNumber
  webCommand $ S.delRuleTemplate rn pn
  seeOther (showRelURL $ Menu Lib gn) $ toResponse "Redirecting..."

-- ** Modules view

viewRuleTemplate :: GameName -> LastRule -> Bool -> RoutedNomyxServer Html
viewRuleTemplate gn (rt, err) isGameAdmin = do
  lf  <- liftRouteT $ lift $ viewForm "user" (submitRuleTemplatForm (Just rt) isGameAdmin)
  ok $ div ! A.class_ "viewrule" $ do
    let pic = fromMaybe "/static/pictures/democracy.png" (_rPicture rt)
    h2 $ fromString $ _rName rt
    img ! (A.src $ toValue $ pic)
    h3 $ fromString $ _rDescription rt
    h2 $ fromString $ "authored by " ++ (_rAuthor rt)
    viewRuleFunc rt
    mapM (viewDecl gn) (_rDeclarations rt)
    div $ pre $ fromString err
    blazeForm lf $ showRelURL (SubmitRule gn)

submitRuleTemplatForm :: (Maybe RuleTemplate) -> Bool -> NomyxForm (String, Maybe String)
submitRuleTemplatForm mrt isGameAdmin = 
   (,) <$> inputHidden (show mrt)
       <*> if isGameAdmin then inputSubmit "Admin submit" else pure Nothing

--submitRuleTemplatForm :: (Maybe RuleTemplate) -> Bool -> NomyxForm (String, Maybe String)
--submitRuleTemplatForm mrt isGameAdmin = do
--  srt <- inputHidden (show mrt)
--  admin <- if isGameAdmin then inputSubmit "Admin submit" else pure Nothing
--  return (srt, admin)

viewRuleFunc :: RuleTemplate -> Html
viewRuleFunc rd = do
  let code = lines $ _rRuleCode rd
  let codeCutLines = 7
  div ! A.id "codeDiv" $ displayCode $ unlines $ take codeCutLines code
  div $ when (length code >= codeCutLines) $ fromString "(...)"

viewDecl :: GameName -> FilePath -> Html
viewDecl gn modPath = do
   let link = showRelURLParams (Menu Modules gn) [("modulePath", Just $ pack $ idEncode modPath)]
   H.a (fromString modPath) ! (A.href $ toValue $ link)


-- | Submit a template to a given game
submitRuleTemplatePost :: GameName -> RoutedNomyxServer Response
submitRuleTemplatePost gn = toResponse <$> do
   methodM POST
   s <- getSession
   let gi = getGameByName gn s
   admin <- isGameAdmin (fromJust gi)
   r <- liftRouteT $ lift $ eitherForm environment "user" (submitRuleTemplatForm Nothing True)
   pn <- fromJust <$> getPlayerNumber
   rt <- case r of
      Right (srt, Nothing) -> do
         let rt = fromJust $ read srt
         webCommand $ submitRule rt pn gn
         return rt
      Right (srt, Just _)  -> do
         let rt = read srt
         webCommand $ adminSubmitRule rt pn gn
         return rt
      (Left _)            -> error "cannot retrieve form data"
   seeOther (showRelURLParams (Menu Lib gn) [("ruleName", Just $ pack $ idEncode $ _rName rt)]) $ "Redirecting..."


-- * Template edit

-- Edit a template
viewRuleTemplateEdit :: LastRule -> GameName -> RoutedNomyxServer Html
viewRuleTemplateEdit lr gn = do
  lf  <- liftRouteT $ lift $ viewForm "user" (newRuleTemplateForm (Just $ fst lr) True)
  ok $ div ! A.class_ "editRule" $ do
    blazeForm lf $ showRelURL $ NewRuleTemplate gn
    pre $ fromString $ snd lr

newRuleTemplateForm :: Maybe RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String)
newRuleTemplateForm sr isGameAdmin = newRuleTemplateForm' (fromMaybe (RuleTemplate "" "" "" "" Nothing [] []) sr) isGameAdmin

--newRuleTemplateForm' :: RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String)
--newRuleTemplateForm' rt isGameAdmin = do
--  rt <- newRuleTemplateForm'' rt
--  chk <- inputSubmit "Check"
--  return (rt, chk)

newRuleTemplateForm' :: RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String)
newRuleTemplateForm' rt isGameAdmin =
  (,) <$> newRuleTemplateForm'' rt
      <*> inputSubmit "Check"

--newRuleTemplateForm'' :: RuleTemplate -> NomyxForm RuleTemplate
--newRuleTemplateForm'' rt@(RuleTemplate name desc code aut pic cat decls) = do
--  name'  <-  RB.label "Name: " ++> RB.inputText name `setAttr` class_ "ruleName"
--  desc'  <- (RB.label "      Short description: " ++> (RB.inputText desc `setAttr` class_ "ruleDescr") <++ RB.br)
--  code'  <-  RB.label "      Code: " ++> textarea 80 15 code `setAttr` class_ "ruleCode" `setAttr` placeholder "Enter here your rule"
--  aut'   <-  (inputHidden aut)
--  pic'   <-  (read <$> (inputHidden $ show pic))
--  cat'   <-  (read <$> (inputHidden $ show cat))
--  decls' <-  (read <$> (inputHidden $ show decls))
--  return (RuleTemplate name' desc' code' aut' pic' cat' decls')

newRuleTemplateForm'' :: RuleTemplate -> NomyxForm RuleTemplate
newRuleTemplateForm'' (RuleTemplate name desc code aut pic cat decls) =
  RuleTemplate <$>  RB.label "Name: " ++> RB.inputText name `setAttr` class_ "ruleName"
               <*> (RB.label "      Short description: " ++> (RB.inputText desc `setAttr` class_ "ruleDescr") <++ RB.br)
               <*>  RB.label "      Code: " ++> textarea 80 15 code `setAttr` class_ "ruleCode" `setAttr` placeholder "Enter here your rule"
               <*>  (inputHidden aut)
               <*>  (read <$> (inputHidden $ show pic))
               <*>  (read <$> (inputHidden $ show cat))
               <*>  (read <$> (inputHidden $ show decls))

newRuleTemplate :: GameName -> RoutedNomyxServer Response
newRuleTemplate gn = toResponse <$> do
  methodM POST
  r <- liftRouteT $ lift $ eitherForm environment "user" (newRuleTemplateForm Nothing False)
  pn <- fromJust <$> getPlayerNumber
  case r of
     Right (rt, Nothing) -> do
       webCommand $ S.newRuleTemplate pn rt
       seeOther (showRelURLParams (Menu Lib gn) [("ruleName", Just $ pack $ idEncode $ _rName rt)]) $ "Redirecting..."
     Right (rt, Just _)  -> do
       webCommand $ S.checkRule rt pn gn
       seeOther (showRelURLParams (Menu Lib gn) [("ruleName", Just $ pack $ idEncode $ _rName rt), ("edit", Nothing)]) $ "Redirecting..."
     _ -> error "cannot retrieve form data"

