{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Nomyx.Web.Game.NewRule where

import Prelude hiding (div)
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import Control.Applicative
import Data.Maybe
import Data.String
import Data.Text (Text)
import Text.Blaze.Html5                    (Html, (!), h3, pre, toValue, a)
import Text.Blaze.Html5.Attributes as A    (id, disabled, placeholder, class_)
import Text.Reform.Blaze.String            (label, textarea, inputSubmit)
import qualified Text.Reform.Blaze.String as RB
import Text.Reform.Happstack               (environment)
import Text.Reform                         ((<++), (++>), viewForm, eitherForm)
import Text.Reform.Blaze.Common            (setAttr)
import Happstack.Server                    (Response, Method(..), seeOther, toResponse, methodM, ok)
import Web.Routes.RouteT                   (showURL, liftRouteT)
import Safe
import qualified Nomyx.Web.Help as Help
import Nomyx.Web.Common as NWC
import Nomyx.Core.Types as T
import Nomyx.Core.Mail
import Nomyx.Core.Engine
import Nomyx.Core.Session as S
import Nomyx.Core.Profile as Profile
default (Integer, Double, Data.Text.Text)

newRuleForm :: Maybe SubmitRule -> Bool -> NomyxForm (SubmitRule, Maybe String, Maybe String)
newRuleForm (Just sr) isGameAdmin = newRuleForm' sr isGameAdmin
newRuleForm Nothing isGameAdmin = newRuleForm' (SubmitRule "" "" "") isGameAdmin

newRuleForm' :: SubmitRule -> Bool -> NomyxForm (SubmitRule, Maybe String, Maybe String)
newRuleForm' (SubmitRule name desc code) isGameAdmin =
   (,,) <$> (SubmitRule <$> label "Name: " ++> RB.inputText name `setAttr` class_ "ruleName"
                        <*> (label "      Short description: " ++> (RB.inputText desc `setAttr` class_ "ruleDescr") <++ RB.br)
                        <*> label "      Code: " ++> textarea 80 15 code `setAttr` class_ "ruleCode" `setAttr` placeholder "Enter here your rule")
       <*> inputSubmit "Check"
       <*> if isGameAdmin then inputSubmit "Admin submit" else pure Nothing


viewRuleForm :: Maybe LastRule -> Bool -> Bool -> GameName -> RoutedNomyxServer Html
viewRuleForm mlr inGame isGameAdmin gn = do
   link <- showURL (NewRule gn)
   lf  <- lift $ viewForm "user" (newRuleForm (fst <$> mlr) isGameAdmin)
   ok $ do
      a "" ! A.id (toValue ruleFormAnchor)
      titleWithHelpIcon (h3 "Propose a new rule:") Help.code
      if inGame then do
         blazeForm lf link
         let msg = snd <$> mlr
         when (isJust msg) $ pre $ fromString $ fromJust msg
      else lf ! disabled ""

newRule :: GameName -> TVar Session -> RoutedNomyxServer Response
newRule gn ts = toResponse <$> do
   methodM POST
   s@(T.Session sh _ _) <- liftIO $ readTVarIO ts
   admin <- getIsAdmin ts
   r <- liftRouteT $ eitherForm environment "user" (newRuleForm Nothing admin)
   link <- showURL MainPage
   pn <- fromJust <$> getPlayerNumber ts
   case r of
       Right (sr, Nothing, Nothing) -> do
          webCommand ts $ submitRule sr pn gn sh
          liftIO $ do
             s' <- readTVarIO ts  --TODO clean this
             gn <- getPlayersGame pn s
             gn' <- getPlayersGame pn s'
             let rs = _rules $ _game $ _loggedGame $ fromJustNote "newRule" gn
             let rs' = _rules $ _game $ _loggedGame $ fromJustNote "newRule" gn'
             when (length rs' > length rs) $ sendMailsNewRule s' sr pn
       Right (sr, Just _, Nothing) -> webCommand ts $ checkRule sr pn sh
       Right (sr, Nothing, Just _) -> webCommand ts $ adminSubmitRule sr pn gn sh
       Right (_,  Just _, Just _)  -> error "Impossible new rule form result"
       (Left _) -> liftIO $ putStrLn "cannot retrieve form data"
   seeOther (link `appendAnchor` ruleFormAnchor) $ "Redirecting..."

