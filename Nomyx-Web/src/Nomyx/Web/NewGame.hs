{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Nomyx.Web.NewGame where

import           Control.Applicative
import           Control.Monad.State
import           Data.Maybe
import           Data.Text                   (Text)
import           Data.List                   
import           Data.String                 (fromString)
import           Happstack.Server
import           Nomyx.Language
import           Nomyx.Core.Engine
import qualified Nomyx.Core.Session          as S
import           Nomyx.Core.Types
import           Nomyx.Web.Common
import           Nomyx.Web.Types
import qualified Nomyx.Web.Help                        as Help
import           Prelude                     hiding (div)
import           Text.Blaze.Html5                      hiding (head, map, br, label, textarea)
import qualified Text.Blaze.Html5                      as H
import           Text.Blaze.Html5.Attributes           hiding (dir, id, label)
import qualified Text.Blaze.Html5.Attributes           as A
import           Text.Reform
import qualified Text.Reform.Blaze.Common    as RBC
import           Text.Reform.Blaze.String    as RB hiding (form)
import           Text.Reform.Happstack
import           Web.Routes.RouteT

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

newGamePage :: RoutedNomyxServer Response
newGamePage = toResponse <$> do
   admin <- isAdmin
   gis <- getPublicGames
   s <- getSession
   mpn <- getPlayerNumber
   let gameNames = map (_gameName . _game . _loggedGame) gis
   mf <- liftRouteT $ lift $ viewForm "user" $ newGameForm admin gameNames
   let gameForm = do
       fromString "Create new game:"
       viewGames (_gameInfos $ _multi s) admin mpn
       blazeForm mf $ showRelURL SubmitNewGame
   mainPage "New game"
            "New game"
            gameForm
            False
            True

newGamePost :: RoutedNomyxServer Response
newGamePost = toResponse <$> do
   methodM POST
   admin <- isAdmin
   gis <- getPublicGames
   let gameNames = map (_gameName . _game . _loggedGame) gis
   r <- liftRouteT $ lift $ eitherForm environment "user" (newGameForm admin gameNames)
   pn <- fromJust <$> getPlayerNumber
   case r of
      Left errorForm -> mainPage  "New game" "New game" (blazeForm errorForm $ showRelURL SubmitNewGame) False True
      Right (NewGameForm name desc isPublic mforkFrom) -> do
         case mforkFrom of
            Nothing       -> webCommand $ S.newGame name desc pn isPublic
            Just forkFrom -> webCommand $ S.forkGame forkFrom name desc False pn
         seeOther (showRelURL NewGame) "Redirecting..."

viewGames :: [GameInfo] -> Bool -> (Maybe PlayerNumber) -> Html
viewGames gis isAdmin mpn = do
   let canCreateGame = maybe False (\pn -> isAdmin || numberOfGamesOwned gis pn < 1) mpn
  -- let publicPrivate = partition ((== True) . _isPublic) gis
   let games = map (viewGameName isAdmin mpn) gis
   b "Games:"
   table $ do
      --p ! A.style "font-weight:bold;"
      sequence_ games
   H.br
   --when canCreateGame $ H.a "Create a new game" ! (href $ toValue $ defLink NewGame (isJust mpn)) >> H.br

viewGameName :: Bool -> (Maybe PlayerNumber) -> GameInfo -> Html
viewGameName isAdmin mpn gi = do
   let g = getGame gi
   let isGameAdmin = isAdmin || maybe False (==mpn) (Just $ _ownedBy gi)
   let gn = _gameName g
   let canView = isGameAdmin || _isPublic gi
   when canView $ tr $ td $ H.a (fromString (gn ++ "   ")) ! (A.title $ toValue Help.view) -- ! attr

