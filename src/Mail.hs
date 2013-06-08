{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


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
import Language.Nomyx.Game
import Data.Text(Text, pack)
import Control.Concurrent
import Data.Maybe
import Utils
import Control.Exception
import qualified Data.Text.Lazy as B
import qualified Language.Haskell.HsColour.HTML as HSC
import Language.Haskell.HsColour.Colourise hiding (string)
import Text.Blaze.Internal
import Control.Category
import qualified Data.Acid.Advanced as A (query')
import Control.Applicative ((<$>))
default (Integer, Double, Data.Text.Text)


sendMail :: String -> String -> String -> IO()
sendMail to object body = do
   putStrLn $ "sending a mail to " ++ to
   forkIO $ simpleMail (Address Nothing (pack to)) (Address (Just "Nomyx Game") "Nomyx.Game@gmail.com") (pack object) "" (B.pack body) [] >>= renderSendMail
   putStrLn $ "done"


newRuleBody :: PlayerName -> SubmitRule -> PlayerName -> Network -> Html
newRuleBody playerName (SubmitRule name desc code) prop net = docTypeHtml $ do
   (toHtml $ "Dear " ++ playerName ++ ",") >> H.br
   (toHtml $ "a new rule has been proposed by player " ++ prop ++ ".") >> H.br
   (toHtml $ "Name: " ++ name) >> H.br
   (toHtml $ "Description: " ++ desc) >> H.br
   (toHtml $ "Code: ") >> H.br >> (preEscapedString $ HSC.hscolour defaultColourPrefs False $ code) >> H.br >> H.br
   (toHtml $ "Please login into Nomyx for actions on this rule:") >> H.br
   (toHtml $ nomyxURL net ++ "/Nomyx") >> H.br >> H.br
   (toHtml $ "You received this mail because you subscribed to Nomyx. To stop receiving mails, login to Nomyx with the above address, go to Settings and uncheck the corresponding box.") >> H.br

newRuleObject :: PlayerName -> String
newRuleObject name = "[Nomyx] New rule posted by player " ++ name ++ "!"

sendMailsNewRule :: Session -> SubmitRule -> PlayerNumber -> IO()
sendMailsNewRule s sr pn = do
   evaluate s
   gn <- fromJust <$> getPlayersGame pn s
   proposer <- getPlayersName pn s
   pfd <- A.query' (acidProfileData $ _profiles s) AskProfilesData
   let pls = [ p { _pPlayerNumber = mypn} | p <- pfd, mypn <- map _playerNumber $ _players $ _game gn]
   forM_ pls $ send proposer (_net $ _mSettings $ _multi s)
   where
      send :: PlayerName -> Network -> ProfileData -> IO()
      send prop net pfd = when (_mailNewRule $ _pPlayerSettings pfd)
          $ sendMail (_mailTo $ _pPlayerSettings $ pfd) (newRuleObject prop) (renderHtml $ newRuleBody (_pPlayerName $ _pPlayerSettings $ pfd) sr prop net)

   
mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f


newRulebody :: Rule -> String
newRulebody (Rule {_rNumber, _rProposedBy}) = "Rule number " ++ (show _rNumber) ++ " has been proposed by player " ++ (show _rProposedBy)



