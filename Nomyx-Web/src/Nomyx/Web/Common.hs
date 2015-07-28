{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module Nomyx.Web.Common where

import           Prelude hiding (div)
import           Safe
import           Text.Blaze.Html5 hiding (map, output, base)
import           Text.Blaze.Html5.Attributes hiding (dir, id)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Web.Routes.PathInfo
import           Web.Routes.RouteT
import           Web.Routes.TH (derivePathInfo)
import           Web.Routes.Happstack()
import           Control.Monad.State
import           Control.Concurrent.STM
import           Happstack.Server as HS
import           Happstack.Authenticate.Core (UserId(..), getUserId, AuthenticateURL(..))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe
import           Data.Text (unpack, append, Text, pack)
import           Data.String
import           Text.Printf
import           Text.Reform.Happstack()
import           Text.Reform (Form, FormError(..), CommonFormError, FormInput, ErrorInputType)
import           Text.Reform.Blaze.String()
import qualified Text.Reform.Generalized as G
import           Language.Haskell.HsColour.HTML      (hscolour)
import           Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import           Language.Nomyx
import           Nomyx.Core.Engine
import           Nomyx.Core.Session
import           Nomyx.Core.Profile
import           Nomyx.Core.Types as T
import           Data.Monoid
import           Language.Javascript.JMacro

data NomyxError = PlayerNameRequired
                | GameNameRequired
                | UniqueName
                | UniqueEmail
                | FieldTooLong Int
                | NomyxCFE (CommonFormError [HS.Input])
                deriving Show

type NomyxForm a = Form (ServerPartT IO) [HS.Input] NomyxError Html () a

default (Integer, Double, Data.Text.Text)

data LoginName = LoginName { login :: PlayerName}
                             deriving (Show, Eq)


-- | associate a player number with a handle
data PlayerClient = PlayerClient PlayerNumber deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server [PlayerClient] deriving (Eq, Show)

data PlayerCommand = --NotLogged
                     Auth AuthenticateURL
                   | Login
                   | ResetPassword
                   | ChangePassword
                   | OpenIdRealm
                   | PostAuth
                   | MainPage
                   | JoinGame  GameName
                   | LeaveGame GameName
                   | DelGame   GameName
                   | DoInput   EventNumber SignalAddress FormField GameName
                   | NewRule   GameName
                   | NewGame
                   | SubmitNewGame
                   | Upload
                   | PlayerSettings
                   | SubmitPlayerSettings
                   | Advanced
                   | SubmitPlayAs GameName
                   | SubmitAdminPass
                   | SubmitSettings
                   | SaveFilePage
                   | NomyxJS
                   deriving (Show)

ruleFormAnchor, inputAnchor :: Text
ruleFormAnchor = "RuleForm"
inputAnchor = "Input"

type RoutedNomyxServer a = RouteT PlayerCommand (ServerPartT IO) a


instance PathInfo SignalAddressElem
instance PathInfo SignalAddress
instance PathInfo FormField
instance PathInfo (Int, String)
instance PathInfo [(Int, String)]

$(derivePathInfo ''PlayerCommand)
$(derivePathInfo ''LoginName)

instance PathInfo Bool where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "bool") (checkBool . show)
   where checkBool str =
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing

blazeResponse :: Html -> Response
blazeResponse html = toResponseBS (C.pack "text/html;charset=UTF-8") $ renderHtml html

blazeForm :: Html -> Text -> Html
blazeForm html link =
    H.form ! A.action (toValue link)
         ! A.method "POST"
         ! A.enctype "multipart/form-data" $
            do html
               input ! A.type_ "submit" ! A.value "Submit"

-- | Create a group of radio elements without BR between elements
inputRadio' :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
              [(a, lbl)]  -- ^ value, label, initially checked
           -> (a -> Bool) -- ^ isDefault
           -> Form m input error Html () a
inputRadio' choices isDefault =
   G.inputChoice isDefault choices mkRadios
   where
      mkRadios nm choices' = mconcat $ concatMap (mkRadio nm) choices'
      mkRadio nm (i, val, lbl, checked) =
         [ (if checked then (! A.checked "checked") else id) $ input ! A.type_ "radio" ! A.id (toValue i) ! A.name (toValue nm) ! A.value (toValue val)
         , " ", H.label ! A.for (toValue i) $ toHtml lbl]

mainPage' :: String -> Html -> Bool -> Html -> RoutedNomyxServer Response
mainPage' title header footer body = do
   html <- mainPage title header body footer False
   return $ toResponse html

mainPage :: String -> Html -> Html -> Bool -> Bool -> RoutedNomyxServer Html
mainPage title header body footer backLink = do
   link <- showURL MainPage
   routeFn <- askRouteFn
   ok $ if backLink then appTemplate' title header body footer (Just $ unpack link) routeFn
                    else appTemplate' title header body footer Nothing routeFn

appTemplate' ::
       String -- ^ title
    -> Html   -- ^ extra tags to include in \<head\>
    -> Html   -- ^ contents to put inside \<body\>
    -> Bool   -- ^ include footer
    -> Maybe String -- ^ link to main page
    -> (URL (RouteT PlayerCommand (ServerPartT IO))
                          -> [(Text, Maybe Text)] -> Text)
    -> Html
appTemplate' title headers body footer link routeFn = do
   H.head $ do
      H.title (fromString title)
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomyx.css"
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
      H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
      H.meta ! A.name "keywords" ! A.content "Nomyx, game, rules, Haskell, auto-reference"
      H.script ! A.type_ "text/JavaScript" ! A.src "/static/nomyx.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "http://ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "http://ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src (textValue $ routeFn NomyxJS []) $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src (textValue $ routeFn (Auth Controllers) []) $ ""
   H.body ! onload "loadDivVisibility()"  ! customAttribute "ng-controller" "AuthenticationCtrl" ! customAttribute "ng-app" "NomyxApp" $ H.div ! A.id "container" $ do
      H.div ! A.id "header" $ table ! width "100%" $ tr $ do
         td (p headers ! A.id "headerTitle")
         when (isJust link) $ td ! A.style "text-align:right;" $ H.a "Back to main page" ! (href $ toValue $ fromJust link)
      body
      when footer $ H.div ! A.id "footer" $ "Copyright Corentin Dupont 2012-2013"

-- appTemplate :: ( Monad m) => String -> Html -> Html -> m Response
-- appTemplate title headers body = return $ toResponse $ appTemplate' title headers body True Nothing

-- | return the player number (user ID) based on the session cookie.
getPlayerNumber :: TVar Session -> RoutedNomyxServer (Maybe PlayerNumber)
getPlayerNumber ts = do
   (T.Session _ _ (Profiles acidAuth _)) <- liftIO $ readTVarIO ts
   uid <- getUserId acidAuth
   case uid of
      Nothing -> return Nothing
      (Just (UserId userID)) -> return $ Just $ fromInteger userID

--update the session using the command and saves it
webCommand :: TVar Session -> StateT Session IO () -> RoutedNomyxServer ()
webCommand ts ss = liftIO $ updateSession ts ss

isAdmin :: TVar Session -> RoutedNomyxServer Bool
isAdmin ts = do
   mpn <- getPlayerNumber ts
   case mpn of
      Just pn -> do
         mpf <- getProfile' ts pn
         case mpf of
            Just pf -> return $ _pIsAdmin pf
            Nothing -> return False
      Nothing -> return False

isGameAdmin :: GameInfo -> TVar Session -> RoutedNomyxServer Bool
isGameAdmin gi ts = do
   mpn <- getPlayerNumber ts
   admin <- isAdmin ts
   return $ case mpn of
      Just pn -> admin || (_ownedBy gi == Just pn)
      Nothing -> False

getPublicGames :: TVar Session -> RoutedNomyxServer [GameInfo]
getPublicGames ts = do
   (T.Session _ m _) <- liftIO $ readTVarIO ts
   return $ filter ((== True) . _isPublic) (_gameInfos $ m)


fieldRequired :: NomyxError -> String -> Either NomyxError String
fieldRequired a []  = Left a
fieldRequired _ str = Right str

appendAnchor :: Text -> Text -> Text
appendAnchor url a = url `append` "#" `append` a

displayCode :: String -> Html
displayCode s = preEscapedToHtml $ hscolour defaultColourPrefs False 0 s

getGame :: GameInfo -> Game
getGame = _game . _loggedGame

numberOfGamesOwned :: [GameInfo] -> PlayerNumber -> Int
numberOfGamesOwned gis pn = length $ filter (maybe False (==pn) . _ownedBy) gis

getFirstGame :: Session -> Maybe GameInfo
getFirstGame = headMay . (filter _isPublic) ._gameInfos . _multi

showHideTitle :: String -> Bool -> Bool -> Html -> Html -> Html
showHideTitle id visible empty title rest = do
   let id' = filter (/=' ') id
   div $ table ! width "100%" $ tr $ do
      td $ div $ title
      td $ div $ a (if visible then "Click to hide" else "Click to show") ! A.id (fromString $ id' ++ "Show") ! A.class_ "button showHide" ! onclick (fromString $ printf "toggle_visibility('%sBody', '%sShow')" id' id')
   div ! A.id (fromString $ id' ++ "Body") ! A.style (fromString $ "display:" ++ (if visible then "block;" else "none;")) $
      if empty then (toHtml "No Rules") else rest

titleWithHelpIcon :: Html -> String -> Html
titleWithHelpIcon myTitle help = table ! width "100%" $ tr $ do
   td ! A.style "text-align:left;" $ myTitle
   td ! A.style "text-align:right;" $ img ! src "/static/pictures/help.jpg" ! A.title (toValue help)

--mapping for the javascript function.
setDivVisibilityAndSave :: String -> String -> String
setDivVisibilityAndSave groupName elementName = printf "setDivVisibilityAndSave('%s', '%s')" groupName elementName

defLink :: PlayerCommand -> Bool -> RoutedNomyxServer Text
defLink a logged = if logged then showURL a else showURL (Auth $ Controllers)

trim = unwords . words

-- | app module for angulasjs
--
-- We just depend on the usernamePassword module
nomyxJS :: TVar Session -> JStat
nomyxJS _ = [jmacro|
 {
   var demoApp = angular.module('NomyxApp', [
     'happstackAuthentication',
     'usernamePassword',
     'openId',
     'ngRoute'
   ]);

   demoApp.config(['$routeProvider',
     function($routeProvider) {
       $routeProvider.when('/resetPassword',
                            { templateUrl: '/authenticate/authentication-methods/password/partial/reset-password-form',
                              controller: 'UsernamePasswordCtrl'
                            });
     }]);

   demoApp.controller('NomyxCtrl', ['$scope', '$http',function($scope, $http) {
     $scope.message = '';

     $scope.callRestricted = function (url) {
       $http({url: url, method: 'GET'}).
       success(function (datum, status, headers, config) {
         $scope.message = datum.name;
       }).
       error(function (datum, status, headers, config) {
         alert(datum);
       });
     };
   }]);
 }
|]


instance FormError NomyxError where
    type ErrorInputType NomyxError = [HS.Input]
    commonFormError = NomyxCFE

instance ToMarkup NomyxError where
    toMarkup PlayerNameRequired = "Player Name is required"
    toMarkup GameNameRequired   = "Game Name is required"
    toMarkup UniqueName         = "Name already taken"
    toMarkup UniqueEmail        = "Email already taken"
    toMarkup (FieldTooLong l)   = fromString $ "Field max length: " ++ show l
    toMarkup (NomyxCFE e)       = fromString $ show e
