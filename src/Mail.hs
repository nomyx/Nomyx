{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


module Mail where

import Text.Blaze.Html5 hiding (map, label, br)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String
import Network.Mail.Mime hiding (mailTo)
import Prelude hiding (div)
import Text.Reform.Happstack()
import Control.Monad
import Types
import Language.Nomyx.Expression
import Data.Text(Text, pack)
import Control.Concurrent
import Data.List
import Data.Maybe
import Utils
import qualified Data.Text.Lazy as B
default (Integer, Double, Data.Text.Text)


sendMail :: String -> String -> String -> IO()
sendMail to object body = do
   putStrLn $ "sending a mail to " ++ to
   forkIO $ simpleMail (Address Nothing (pack to)) (Address (Just "Nomyx Game") "corentin.dupont@gmail.com") (pack object) "" (B.pack body) [] >>= renderSendMail
   putStrLn $ "done"
   return ()

newRuleBody :: PlayerName -> SubmitRule -> PlayerName -> Network -> Html
newRuleBody playerName (SubmitRule name desc code) prop net = docTypeHtml $ do
   (toHtml $ "Dear " ++ playerName ++ ",") >> H.br
   (toHtml $ "a new rule has been proposed by player " ++ prop ++ ".") >> H.br
   (toHtml $ "Name: " ++ name) >> H.br
   (toHtml $ "Description: " ++ desc) >> H.br
   (toHtml $ "Code: " ++ code) >> H.br >> H.br
   (toHtml $ "Please login into Nomyx for actions on this rule:") >> H.br
   (toHtml $ nomyxURL net ++ "/Nomyx") >> H.br >> H.br
   (toHtml $ "You received this mail because you subscribed to Nomyx. To stop receiving mails, login to Nomyx with the above address, go to Settings and uncheck the corresponding box.") >> H.br

newRuleObject :: PlayerName -> String
newRuleObject name = "[Nomyx] New rule posted by player " ++ name ++ "!"

sendMailsNewRule :: Multi -> SubmitRule -> PlayerNumber -> IO()
sendMailsNewRule m sr pn = do
   let gn = gameName $ fromJust $ getPlayersGame pn m
   let proposer = getPlayersName pn m
   forM_ (mPlayers m) $ send proposer gn
   where send prop gn pm = when ((Just gn == inGame pm) && (mailNewRule $ mMail pm)) $ \
      sendMail (mailTo $ mMail pm) (newRuleObject prop) (renderHtml $ newRuleBody (mPlayerName pm) sr prop (net m))


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



