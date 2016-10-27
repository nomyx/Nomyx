{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}


module Nomyx.Core.Mail where

import           Control.Category
import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import           Data.Text                           (Text, pack)
import qualified Data.Text.Lazy                      as B
import           Language.Haskell.HsColour.Colourise hiding (string)
import qualified Language.Haskell.HsColour.HTML      as HSC
import           Language.Nomyx
import           Network.Mail.Mime                   hiding (mailTo)
import           Nomyx.Core.Engine
import           Nomyx.Core.Profile
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           Prelude                             hiding (div, (.))
import           Text.Blaze.Html.Renderer.String
import           Text.Blaze.Html5                    hiding (br, label, map)
import qualified Text.Blaze.Html5                    as H

default (Integer, Double, Data.Text.Text)


sendMail :: String -> String -> String -> String-> IO()
sendMail to obj htmlBody textBody = do
   putStrLn $ "sending a mail to " ++ to
   forkIO $ simpleMail (Address Nothing (pack to))
                       (Address (Just "Nomyx Game") "kau@nomyx.net")
                       (pack obj)
                       (B.pack htmlBody)
                       (B.pack textBody)
                       [] >>= renderSendMail
   putStrLn "done"


newRuleHtmlBody :: PlayerName -> RuleTemplate -> PlayerName -> Network -> Html
newRuleHtmlBody playerName (RuleTemplate name desc code _ _ _ _) prop net = docTypeHtml $ do
   toHtml ("Dear " ++ playerName ++ ",") >> H.br
   toHtml ("a new rule has been proposed by player " ++ prop ++ ".") >> H.br
   toHtml ("Name: " ++ name) >> H.br
   toHtml ("Description: " ++ desc) >> H.br
   toHtml "Code: " >> H.br >> (preEscapedToHtml $ HSC.hscolour defaultColourPrefs False 0 code) >> H.br >> H.br
   toHtml "Please login into Nomyx for actions on this rule:" >> H.br
   toHtml (nomyxURL net ++ "/Nomyx") >> H.br >> H.br
   toHtml "You received this mail because you subscribed to Nomyx. To stop receiving mails, login to Nomyx with the above address, go to Settings and uncheck the corresponding box." >> H.br

newRuleTextBody :: PlayerName -> RuleTemplate -> PlayerName -> Network -> String
newRuleTextBody playerName (RuleTemplate name desc code _ _ _ _) prop net =
   "Dear " ++ playerName ++ ",\n" ++
   "a new rule has been proposed by player " ++ prop ++ ".\n" ++
   "Name: " ++ name ++ "\n" ++
   "Description: " ++ desc ++ "\n" ++
   "Code: \n" ++ code ++ "\n\n" ++
   "Please login into Nomyx for actions on this rule:\n" ++
   nomyxURL net ++ "/Nomyx\n\n" ++
   "You received this mail because you subscribed to Nomyx. To stop receiving mails, login to Nomyx with the above address, go to Settings and uncheck the corresponding box.\n"


newRuleObject :: PlayerName -> String
newRuleObject name = "[Nomyx] New rule posted by player " ++ name ++ "!"

sendMailsSubmitRule :: Session -> RuleTemplate -> PlayerNumber -> GameName -> IO ()
sendMailsSubmitRule s sr pn gn = when (_sendMails $ _mSettings $ _multi s) $ do
   let (Just gi) = getGameByName gn s
   guard (_isPublic gi)
   putStrLn "Sending mails"
   let sendMailsTo = map _playerNumber (_players $ _game $ _loggedGame gi)
   proposer <- Nomyx.Core.Profile.getPlayerName pn s
   profiles <- mapM (getProfile s) sendMailsTo
   mapM_ (send proposer (_net $ _mSettings $ _multi s) sr) (_pPlayerSettings <$> catMaybes profiles)


send :: PlayerName -> Network -> RuleTemplate -> PlayerSettings -> IO()
send prop net sr set = when (_mailSubmitRule set && (isJust $ _mail set))
   $ sendMail (fromJust $ _mail set)
              (newRuleObject prop)
              (newRuleTextBody (_pPlayerName set) sr prop net)
              (renderHtml $ newRuleHtmlBody (_pPlayerName set) sr prop net)

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f
