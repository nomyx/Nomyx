{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nomyx.Web.Common where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.State
import           Data.Acid.Advanced                  (query')
import qualified Data.ByteString.Char8               as C
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                           (Text, append, tail,
                                                      unpack)
import           Happstack.Authenticate.Core         (AuthenticateURL (..),
                                                      GetUserByUserId (..),
                                                      User, UserId (..),
                                                      getUserId)
import           Happstack.Server                    as HS
import           Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import           Language.Haskell.HsColour.HTML      (hscolour)
import           Language.Javascript.JMacro          (JStat (..), jLam, jVarTy,
                                                      jhFromList, jmacro,
                                                      toJExpr)
import           Network.HTTP.Types                  (urlEncode)
import           Nomyx.Language
import           Nomyx.Core.Engine
import           Nomyx.Core.Profile
import           Nomyx.Core.Session
import           Nomyx.Core.Types                    as T
import           Nomyx.Web.Types
import qualified Nomyx.Auth                          as Auth
import           Numeric
import           Prelude                             hiding (div)
import           Safe
import           Text.Blaze.Html.Renderer.Utf8       (renderHtml)
import           Text.Blaze.Html5                    hiding (base, map, output)
import qualified Text.Blaze.Html5                    as H
import           Text.Blaze.Html5.Attributes         hiding (dir, id)
import qualified Text.Blaze.Html5.Attributes         as A
import           Text.Printf
import           Text.Reform                         (ErrorInputType, Form,
                                                      FormError (..), FormInput)
import           Text.Reform.Blaze.String            ()
import qualified Text.Reform.Generalized             as G
import           Text.Reform.Happstack               ()
import           Web.Routes.Base
import           Web.Routes.Happstack                ()
import           Web.Routes.RouteT
import           Web.Routes.PathInfo
import           Imprevu.Happstack.Types hiding (updateSession)


ruleFormAnchor, inputAnchor :: Text
ruleFormAnchor = "RuleForm"
inputAnchor = "Input"

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
   routeFn <- askRouteFn
   ok $ pageTemplate title header body footer routeFn

pageTemplate ::
       String -- ^ title
    -> Html   -- ^ extra tags to include in \<head\>
    -> Html   -- ^ contents to put inside \<body\>
    -> Bool   -- ^ include footer
    -> (URL (RouteT PlayerCommand (ServerPartT IO))
                          -> [(Text, Maybe Text)] -> Text)
    -> Html
pageTemplate title headers body footer routeFn = do
 H.html ! manifest "nomyx.appcache" $ do  
   H.head $ do
      H.title (fromString title)
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomyx.css"
      --H.link ! rel "stylesheet" ! type_ "text/css" ! href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
      H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
      H.meta ! A.name "keywords" ! A.content "Nomyx, game, rules, Haskell, auto-reference"
      H.script ! A.type_ "text/JavaScript" ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "http://ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "http://ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src "/static/nomyx.js" $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src (textValue $ routeFn NomyxJS []) $ ""
      H.script ! A.type_ "text/JavaScript" ! A.src (textValue $ routeFn (Auth Controllers) []) $ ""
   H.body ! onload "loadDivVisibility()"  ! customAttribute "ng-controller" "AuthenticationCtrl" ! customAttribute "ng-app" "NomyxApp" $ H.div ! A.id "container" $ do
      H.div ! A.id "header" $ headers
      body ! A.id "multi" 
      when footer $ H.div ! A.id "footer" $ "Copyright Corentin Dupont 2012-2013"

-- | return the player number (user ID) based on the session cookie.
getPlayerNumber :: RoutedNomyxServer (Maybe PlayerNumber)
getPlayerNumber = do
  auth <- use authState
  liftRouteT $ lift $ Auth.getPlayerNumber auth

getUser :: RoutedNomyxServer (Maybe User)
getUser = do
  auth <- use authState
  liftRouteT $ lift $ Auth.getUser auth

getProfile' :: PlayerNumber -> RoutedNomyxServer (Maybe ProfileData)
getProfile' pn = do
  s <- getSession
  getProfile s pn

getSession :: RoutedNomyxServer Session
getSession = do
  ts <- use webSession
  liftIO $ atomically $ readTVar ts

--update the session using the command and saves it
webCommand :: StateT Session IO () -> RoutedNomyxServer ()
webCommand ss = do
  ts <- use webSession
  liftIO $ updateSession ts ss

isAdmin :: RoutedNomyxServer Bool
isAdmin = do
   mpn <- getPlayerNumber
   case mpn of
      Just pn -> do
         mpf <- getProfile' pn
         case mpf of
            Just pf -> return $ _pIsAdmin pf
            Nothing -> return False
      Nothing -> return False

isGameAdmin :: GameInfo -> RoutedNomyxServer Bool
isGameAdmin gi = do
   mpn <- getPlayerNumber
   admin <- isAdmin
   return $ case mpn of
      Just pn -> admin || (_ownedBy gi == Just pn)
      Nothing -> False

getPublicGames :: RoutedNomyxServer [GameInfo]
getPublicGames = do
   s <- getSession
   return $ filter ((== True) . _isPublic) (_gameInfos $ _multi s)

fieldRequired :: NomyxError -> String -> Either NomyxError String
fieldRequired a []  = Left a
fieldRequired _ str = Right str

idEncode :: String -> String
idEncode t = map (\c -> if c==' ' then '+'; else c) t

displayCode :: String -> Html
displayCode s = preEscapedToHtml $ hscolour defaultColourPrefs False 0 s

getGame :: GameInfo -> Game
getGame = _game . _loggedGame

numberOfGamesOwned :: [GameInfo] -> PlayerNumber -> Int
numberOfGamesOwned gis pn = length $ filter (maybe False (==pn) . _ownedBy) gis

getFirstGame :: Session -> Maybe GameInfo
getFirstGame = headMay . filter _isPublic ._gameInfos . _multi

titleWithHelpIcon :: Html -> String -> Html
titleWithHelpIcon myTitle help = table ! width "100%" $ tr $ do
   td ! A.style "text-align:left;" $ myTitle
   td ! A.style "text-align:right;" $ img ! src "/static/pictures/help.jpg" ! A.title (toValue help)

defLink :: PlayerCommand -> Bool -> Text
defLink a logged = if logged then showRelURL a else showRelURL Login

trim = unwords . words

showRelURL :: PlayerCommand -> Text
showRelURL c = "/Nomyx" <> (toPathInfo c)

showRelURLParams :: PlayerCommand -> [(Text, Maybe Text)] -> Text
showRelURLParams c ps = "/Nomyx" <> (toPathInfoParams c ps)

-- | app module for angulasjs
--
-- We just depend on the usernamePassword module
nomyxJS :: JStat
nomyxJS = [jmacro|
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
