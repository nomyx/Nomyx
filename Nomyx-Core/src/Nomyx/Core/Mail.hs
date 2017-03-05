{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}


module Nomyx.Core.Mail (
   sendMailsSubmitRule
   ) where

import           Prelude                             hiding (div, (.), error)
import           Control.Category
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.Text                           (Text, pack)
import qualified Data.Text.Lazy                      as B
import           Language.Haskell.HsColour.Colourise hiding (string)
import qualified Language.Haskell.HsColour.HTML      as HSC
import           Network.Mail.Mime                   hiding (mailTo)
import           Network.HaskellNet.Auth
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Nomyx.Language
import           Nomyx.Core.Engine
import           Nomyx.Core.Profile
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           Text.Blaze.Html.Renderer.String
import           Text.Blaze.Html5                    hiding (br, label, map)
import qualified Text.Blaze.Html5                    as H
import           System.Log.Logger

default (Integer, Double, Data.Text.Text)


sendMail' :: String -> String -> String -> String -> MailSettings -> IO ()
sendMail' to obj textBody htmlBody (MailSettings _ host login pass) = do
   info $ "Sending a mail to " ++ to ++ " login: " ++ login ++ " pass: " ++ pass
   let authSend connection = do 
       succeeded  <- authenticate LOGIN login pass connection
       if succeeded
         then sendMimeMail to "game@nomyx.net" obj (B.pack textBody) (B.pack htmlBody) [] connection
         else error "Mail authentication error"
   void $ forkIO $ doSMTPSSL host authSend


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
sendMailsSubmitRule s rt pn gn = do 
   let mailSet = _mailSettings $  _mSettings $ _multi s
   when (_sendMails mailSet) $ do
     let (Just gi) = getGameByName gn s
     guard (_isPublic gi)
     putStrLn "Sending mails"
     let sendMailsTo = map _playerNumber (_players $ _game $ _loggedGame gi)
     proposer <- Nomyx.Core.Profile.getPlayerName pn s
     profiles <- mapM (getProfile s) sendMailsTo
     mapM_ (send proposer (_net $ _mSettings $ _multi s) rt mailSet) (_pPlayerSettings <$> catMaybes profiles)


send :: PlayerName -> Network -> RuleTemplate -> MailSettings -> PlayerSettings -> IO()
send prop net sr ms set = when (_mailSubmitRule set && (isJust $ _mail set))
   $ sendMail' (fromJust $ _mail set)
              (newRuleObject prop)
              (newRuleTextBody (_pPlayerName set) sr prop net)
              (renderHtml $ newRuleHtmlBody (_pPlayerName set) sr prop net)
              ms

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

error, info :: (MonadIO m) => String -> m ()
info s = liftIO $ infoM "Nomyx.Core.Mail" s
error s = liftIO $ errorM "Nomyx.Core.Mail" s
