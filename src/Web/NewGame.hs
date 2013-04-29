{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


module Web.NewGame where

import Text.Blaze.Html5 hiding (map, label, br, textarea)
import Prelude hiding (div)
import Text.Reform
import Text.Blaze.Html5.Attributes hiding (label)
import Text.Reform.Blaze.String hiding (form)
import qualified Text.Reform.Blaze.Common as RBC
import Text.Reform.Happstack()
import Control.Applicative
import Types
import Happstack.Server
import Text.Reform.Happstack
import Web.Common
import Control.Monad.State
import Language.Nomyx.Expression
import Web.Routes.RouteT
import Control.Concurrent.STM
import Data.Text(Text)
import Text.Blaze.Internal(string)
import Multi
default (Integer, Double, Data.Text.Text)


data NewGameForm = NewGameForm GameName GameDesc

newGameForm :: NomyxForm NewGameForm
newGameForm = pure NewGameForm <*> (br ++> label "Enter new game name: " ++> (inputText "") `RBC.setAttr` placeholder "Game name"  <++ br <++ br)
                                 <*> newGameDesc

newGameDesc :: NomyxForm GameDesc
newGameDesc = pure GameDesc <*> label "Enter game description:" ++> br ++> (textarea 40 3 "") `RBC.setAttr` placeholder "Enter game description" `RBC.setAttr` class_ "gameDesc" <++ br <++ br
                             <*> label "Enter a link to an agora (e.g. a forum, a mailing list...) where the players can discuss their rules: " ++> br ++> (inputText "") `RBC.setAttr` placeholder "Agora URL (including http://...)" `RBC.setAttr` class_ "agora" <++ br <++ br

newGamePage :: PlayerNumber -> RoutedNomyxServer Html
newGamePage pn = do
   newGameLink <- showURL (SubmitNewGame pn)
   mf <- lift $ viewForm "user" $ newGameForm
   mainPage (blazeForm mf newGameLink)
             "New game"
             "New game"
             False

newGamePost :: PlayerNumber -> (TVar Session) -> RoutedNomyxServer Html
newGamePost pn tm = do
   methodM POST
   r <- liftRouteT $ eitherForm environment "user" newGameForm
   link <- showURL $ Noop pn
   case r of
      Left _ -> error $ "error: newGame"
      Right (NewGameForm name desc) -> webCommand tm pn $ newGame name desc pn
   seeOther link $ string "Redirecting..."
