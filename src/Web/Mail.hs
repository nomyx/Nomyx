{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules#-}


module Web.Mail where

import Network.Mail.Mime
import Prelude hiding (div)
import Text.Reform
import Text.Reform.Blaze.String hiding (form)
import Text.Reform.Happstack()
import Control.Applicative
import Data.Text hiding (map, zip, concatMap)
import Types
default (Integer, Double, Data.Text.Text)



sendMail = simpleMail (Address Nothing "corentin.dupont@gmail.com") (Address Nothing "corentin.dupont@gmail.com") "test4" "test4" "test4" []  >>= renderSendMail

mailForm :: NomyxForm MailSettings
mailForm = pure MailSettings <*> label "Please enter your mail: " ++> (Just <$> (inputText ""))
                             <*> label " send mail on new input needed from you: " ++> inputCheckbox True <++ label " "
                             <*> label " send mail on new rule proposed: " ++> inputCheckbox True <++ label " "
                             <*> label " send mail on new output: " ++> inputCheckbox True <++ label " "
                             <*> pure True


