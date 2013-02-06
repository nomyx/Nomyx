{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


module Web.Mail where

import Text.Blaze.Html5 hiding (map, label)
import Text.Blaze.Html5.Attributes hiding (dir, id, label)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.Mail.Mime hiding (mailTo)
import Prelude hiding (div)
import Text.Reform
import Text.Reform.Blaze.String hiding (form)
import Text.Reform.Happstack()
import Control.Applicative
import Control.Monad
import Types
import Web.Common
import Control.Monad.State
import Language.Nomyx.Expression
import Web.Routes.Site
import Web.Routes.PathInfo
import Web.Routes.Happstack
import Web.Routes.RouteT
import Text.Blaze.Internal
import Data.Text(Text, pack)
import Control.Concurrent
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as B
default (Integer, Double, Data.Text.Text)


sendMail :: String -> String -> String -> IO()
sendMail to object body = do
   forkIO $ simpleMail (Address (Just "Nomyx Game") "Game@Nomyx.com") (Address Nothing (pack to)) (pack object) "" (B.pack body) [] >>= renderSendMail
   return ()

newRuleBody :: Rule -> String
newRuleBody (Rule {rNumber, rProposedBy}) = "Rule number " ++ (show rNumber) ++ " has been proposed by player " ++ (show rProposedBy)

sendMailsNewRule :: [PlayerMulti] -> Rule -> GameName -> IO()
sendMailsNewRule pms r gn = do
   forM_ pms send
   where send pm = when ((Just gn == inGame pm) && (mailNewRule $ mMail pm)) $ sendMail (mailTo $ mMail pm) (newRuleBody r) (newRuleBody r)


outputBody :: String -> String
outputBody o = "New message for you: " ++ o

sendMailsOutputs :: Multi -> Multi -> IO ()
sendMailsOutputs before after = do
   let newOutputs = (getOutputs after) \\ (getOutputs before)
   mapM_  (send $ mPlayers after) newOutputs
   where
   send pms (pn, o) = do
      let mail = mMail $ fromJust $ find (\PlayerMulti {mPlayerNumber} -> pn==mPlayerNumber) pms
      when (mailNewOutput mail) $ sendMail (mailTo mail) (outputBody o) (outputBody o)

   
mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

getOutputs :: Multi -> [Output]
getOutputs m = concatMap outputs $ games m

newRulebody :: Rule -> String
newRulebody (Rule {rNumber, rProposedBy}) = "Rule number " ++ (show rNumber) ++ " has been proposed by player " ++ (show rProposedBy)

mailForm :: NomyxForm MailSettings
mailForm = pure MailSettings <*> label "Please enter your mail: " ++> inputText ""
                             <*> label " send mail on new input needed from you: " ++> inputCheckbox True <++ label " "
                             <*> label " send mail on new rule proposed: " ++> inputCheckbox True <++ label " "
                             <*> label " send mail on new output: " ++> inputCheckbox True <++ label " "
                             <*> pure True

mailSettingsPage :: PlayerNumber -> RoutedNomyxServer Html
mailSettingsPage pn = do
   settingsLink <- showURL (SubmitSettings pn)
   mf <- lift $ viewForm "user" mailForm
   mainPage (blazeForm mf settingsLink)
             "Player settings"
             "Player settings"
             False


