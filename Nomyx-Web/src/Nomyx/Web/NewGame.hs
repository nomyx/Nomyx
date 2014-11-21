{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Nomyx.Web.NewGame where

import Prelude hiding (div)
import Text.Reform
import Text.Blaze.Html5.Attributes hiding (label)
import Text.Reform.Blaze.String as RB hiding (form)
import Text.Reform.Happstack
import qualified Text.Reform.Blaze.Common as RBC
import Control.Applicative
import Control.Monad.State
import Control.Concurrent.STM
import Happstack.Server
import Web.Routes.RouteT
import Data.Text(Text)
import Data.Maybe
import Nomyx.Core.Engine
import Nomyx.Web.Common
import qualified Nomyx.Core.Session as S
import Nomyx.Core.Types
default (Integer, Double, Data.Text.Text)

data NewGameForm = NewGameForm GameName GameDesc Bool (Maybe GameName)

newGameForm :: Bool -> [GameName] -> NomyxForm NewGameForm
newGameForm admin gns = pure NewGameForm
   <*> (br ++> errorList ++> label "Enter new game name: " ++> RB.inputText "" `transformEither` fieldRequired GameNameRequired `RBC.setAttr` placeholder "Game name"  <++ br <++ br)
   <*> newGameDesc
   <*> (if admin then label "Public game? " ++> (RB.inputCheckbox True) else pure False) <++ br
   <*> (label "Fork from existing game (optional):" ++> RB.select ((Nothing, "--") : (map (\gn -> (Just gn, gn)) gns)) isNothing) <++ br <++ br

newGameDesc :: NomyxForm GameDesc
newGameDesc = pure GameDesc <*> label "Enter game description:" ++> br ++> textarea 40 3 "" `RBC.setAttr` placeholder "Enter game description" `RBC.setAttr` class_ "gameDesc" <++ br <++ br
                            <*> label "Enter a link to a place where the players can discuss their rules (e.g. a forum, a mailing list...): " ++> br ++> RB.inputText "" `RBC.setAttr` placeholder "Forum URL (including http://...)" `RBC.setAttr` class_ "forum" <++ br <++ br

gameNameRequired :: String -> Either NomyxError String
gameNameRequired = fieldRequired GameNameRequired

newGamePage :: TVar Session -> RoutedNomyxServer Response
newGamePage ts = toResponse <$> do
   admin <- isAdmin ts
   gis <- getPublicGames ts
   let gameNames = map (_gameName . _game . _loggedGame) gis
   newGameLink <- showURL SubmitNewGame
   mf <- lift $ viewForm "user" $ newGameForm admin gameNames
   mainPage "New game"
            "New game"
            (blazeForm mf newGameLink)
            False
            True

newGamePost :: TVar Session -> RoutedNomyxServer Response
newGamePost ts = toResponse <$> do
   methodM POST
   admin <- isAdmin ts
   gis <- getPublicGames ts
   let gameNames = map (_gameName . _game . _loggedGame) gis
   r <- liftRouteT $ eitherForm environment "user" (newGameForm admin gameNames)
   link <- showURL MainPage
   newGameLink <- showURL SubmitNewGame
   pn <- fromJust <$> getPlayerNumber ts
   case r of
      Left errorForm -> mainPage  "New game" "New game" (blazeForm errorForm newGameLink) False True
      Right (NewGameForm name desc isPublic mforkFrom) -> do
         case mforkFrom of
            Nothing       -> webCommand ts $ S.newGame name desc pn isPublic
            Just forkFrom -> webCommand ts $ S.forkGame forkFrom name desc False pn
         seeOther link "Redirecting..."
