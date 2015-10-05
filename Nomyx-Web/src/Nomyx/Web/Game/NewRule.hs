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
import           Text.Blaze.Html5            (Html, a, h3, pre, toValue, (!))
import           Text.Blaze.Html5.Attributes as A (class_, disabled, id,
                                                   placeholder)
import           Text.Reform                 (eitherForm, viewForm, (++>),
                                              (<++))
import           Text.Reform.Blaze.Common    (setAttr)
import           Text.Reform.Blaze.String    (inputSubmit, label, textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT, showURL)
default (Integer, Double, Data.Text.Text)

newRuleForm :: Maybe RuleDetails -> Bool -> NomyxForm (RuleDetails, Maybe String, Maybe String)
newRuleForm (Just sr) isGameAdmin = newRuleForm' sr isGameAdmin
newRuleForm Nothing isGameAdmin = newRuleForm' (RuleDetails "" "" "" "" Nothing []) isGameAdmin

newRuleForm' :: RuleDetails -> Bool -> NomyxForm (RuleDetails, Maybe String, Maybe String)
newRuleForm' (RuleDetails name desc code "" Nothing []) isGameAdmin =
   (,,) <$> submitRuleForm name desc code
        <*> inputSubmit "Check"
        <*> if isGameAdmin then inputSubmit "Admin submit" else pure Nothing

submitRuleForm :: String -> String -> String -> NomyxForm RuleDetails
submitRuleForm name desc code =
   RuleDetails <$> label "Name: " ++> RB.inputText name `setAttr` class_ "ruleName"
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
