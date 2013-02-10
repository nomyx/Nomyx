{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


module Mail where

import Text.Blaze.Html5 hiding (map, label, br)
import Text.Blaze.Html5.Attributes hiding (dir, id, label)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
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
import Control.Concurrent.STM
import Network.BSD
import Utils
import qualified Data.Text.Lazy as B
default (Integer, Double, Data.Text.Text)



sendMail :: String -> String -> String -> IO()
sendMail to object body = do
   putStrLn $ "sending a mail to " ++ to
   forkIO $ simpleMail (Address Nothing (pack to)) (Address (Just "Nomyx Game") "corentin.dupont@gmail.com") (pack object) "" (B.pack body) [] >>= renderSendMail
   return ()

newRuleBody :: PlayerName -> RuleName -> String -> RuleCode -> PlayerName -> Network -> Html
newRuleBody playerName name description ruleCode prop net = docTypeHtml $ do
   (toHtml $ "Dear " ++ playerName ++ ",") >> H.br
   (toHtml $ "a new rule has been proposed by player " ++ prop ++ ".") >> H.br
   (toHtml $ "Name: " ++ name) >> H.br
   (toHtml $ "Description: " ++ description) >> H.br
   (toHtml $ "Code: " ++ ruleCode) >> H.br >> H.br
   (toHtml $ "Please login into Nomyx for actions on this rule:") >> H.br
   (toHtml $ nomyxURL net ++ "/Nomyx") >> H.br
   (toHtml $ "You received this mail because you subscribed to Nomyx. To stop receiving mails, login to Nomyx with the above address, go to Settings and uncheck the corresponding box") >> H.br

newRuleObject :: PlayerName -> String
newRuleObject name = "[Nomyx] New rule posted by player " ++ name ++ "!"

sendMailsNewRule :: Multi -> RuleName -> String -> RuleCode -> PlayerNumber -> IO()
sendMailsNewRule m name description code pn = do
   let gn = gameName $ fromJust $ getPlayersGame pn m
   let proposer = getPlayersName pn m
   forM_ (mPlayers m) $ send proposer gn
   where send prop gn pm = when ((Just gn == inGame pm) && (mailNewRule $ mMail pm)) $ \
      sendMail (mailTo $ mMail pm) (newRuleObject prop) (renderHtml $ newRuleBody (mPlayerName pm) name description code prop (net m))


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



