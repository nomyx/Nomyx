{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}


module Mail where

import Text.Blaze.Html5 hiding (map, label, br)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String
import Network.Mail.Mime hiding (mailTo)
import Prelude hiding (div, (.))
import Text.Reform.Happstack()
import Control.Monad
import Types
import Language.Nomyx
import Language.Nomyx.Engine
import Data.Text(Text, pack)
import Control.Concurrent
import Data.Maybe
import Utils
import qualified Data.Text.Lazy as B
import qualified Language.Haskell.HsColour.HTML as HSC
import Language.Haskell.HsColour.Colourise hiding (string)
import Text.Blaze.Internal
import Control.Category
import Control.Applicative ((<$>))
import Safe
import Data.List
import Profile
default (Integer, Double, Data.Text.Text)


sendMail :: String -> String -> String -> String-> IO()
sendMail to object htmlBody textBody = do
   putStrLn $ "sending a mail to " ++ to
   forkIO $ simpleMail (Address Nothing (pack to))
                       (Address (Just "Nomyx Game") "kau@nomyx.net")
                       (pack object)
                       (B.pack htmlBody)
                       (B.pack textBody)
                       [] >>= renderSendMail
   putStrLn $ "done"


newRuleHtmlBody :: PlayerName -> SubmitRule -> PlayerName -> Network -> Html
newRuleHtmlBody playerName (SubmitRule name desc code) prop net = docTypeHtml $ do
   (toHtml $ "Dear " ++ playerName ++ ",") >> H.br
   (toHtml $ "a new rule has been proposed by player " ++ prop ++ ".") >> H.br
   (toHtml $ "Name: " ++ name) >> H.br
   (toHtml $ "Description: " ++ desc) >> H.br
   (toHtml $ "Code: ") >> H.br >> (preEscapedString $ HSC.hscolour defaultColourPrefs False $ code) >> H.br >> H.br
   (toHtml $ "Please login into Nomyx for actions on this rule:") >> H.br
   (toHtml $ nomyxURL net ++ "/Nomyx") >> H.br >> H.br
   (toHtml $ "You received this mail because you subscribed to Nomyx. To stop receiving mails, login to Nomyx with the above address, go to Settings and uncheck the corresponding box.") >> H.br

newRuleTextBody :: PlayerName -> SubmitRule -> PlayerName -> Network -> String
newRuleTextBody playerName (SubmitRule name desc code) prop net =
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

sendMailsNewRule :: Session -> SubmitRule -> PlayerNumber -> IO ()
sendMailsNewRule s sr pn = when (_sendMails $ _mSettings $ _multi s) $ do
   putStrLn "Sending mails"
   gn <- fromJustNote "sendMailsNewRule" <$> getPlayersGame pn s
   let sendMailsTo = delete pn (map _playerNumber (_players $ _game $ _loggedGame gn))
   proposer <- Profile.getPlayerName pn s
   profiles <- mapM (getProfile s) sendMailsTo
   mapM_ (send proposer (_net $ _mSettings $ _multi s) sr) (_pPlayerSettings <$> catMaybes profiles)


send :: PlayerName -> Network -> SubmitRule -> PlayerSettings -> IO()
send prop net sr set = when (_mailNewRule set)
   $ sendMail (_mail set)
              (newRuleObject prop)
              (newRuleTextBody (_pPlayerName set) sr prop net)
              (renderHtml $ newRuleHtmlBody (_pPlayerName set) sr prop net)
              
mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f


