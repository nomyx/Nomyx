{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, ScopedTypeVariables, DeriveDataTypeable,
             RecordWildCards, TypeFamilies#-}

module Web.Common where


import Prelude hiding (div, catch)
import Text.Blaze.Html5 hiding (map, output, base)
import Text.Blaze.Html5.Attributes hiding (dir, id)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.PathInfo
import Web.Routes.RouteT
import Web.Routes.TH (derivePathInfo)
import Control.Monad.State
import Control.Concurrent.STM
import Language.Nomyx.Expression
import Happstack.Server
import Types as T
import qualified Data.ByteString.Char8 as C
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Reform.Happstack()
import Text.Reform
import Text.Reform.Blaze.String()
import Text.Reform.Happstack()
import Text.Blaze.Internal
import qualified Text.Reform.Generalized as G
import Data.Text(Text, pack)
import Web.Routes.Happstack()
import Data.Generics         (Data, Typeable)
import Happstack.Auth
       (UserId, AuthProfileURL)


--import Web.Routes.TH         (derivePathInfo)
default (Integer, Double, Data.Text.Text)


data ProfileDataURL
    = CreateNewProfileData
    | ViewProfileData UserId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ProfileDataURL)


data LoginName = LoginName { login :: PlayerName}
                             deriving (Show, Eq)


-- | associate a player number with a handle
data PlayerClient = PlayerClient PlayerNumber deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server [PlayerClient] deriving (Eq, Show)


data PlayerCommand =
                     HomePage
                   | U_AuthProfile AuthProfileURL
                   | U_ProfileData ProfileDataURL

                   | Noop            PlayerNumber
                   | ViewGame        PlayerNumber GameName
                   | JoinGame        PlayerNumber GameName
                   | LeaveGame       PlayerNumber GameName
                   | DoInputChoice   PlayerNumber EventNumber
                   | DoInputString   PlayerNumber String
                   | NewRule         PlayerNumber
                   | NewGame         PlayerNumber
                   | SubmitNewGame   PlayerNumber
                   | Upload          PlayerNumber
                   | PSettings       PlayerNumber
                   | SubmitPlayerSettings  PlayerNumber
                   deriving (Show)


type RoutedNomyxServer a = RouteT PlayerCommand (ServerPartT IO) a

$(derivePathInfo ''PlayerCommand)
$(derivePathInfo ''LoginName)

instance PathInfo Bool where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "bool") (checkBool . show)
   where checkBool str =
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing

modDir :: FilePath
modDir = "modules"

evalCommand :: (TVar Session) -> StateT Session IO a -> RoutedNomyxServer a
evalCommand ts sm = liftRouteT $ lift $ do
   s <- atomically $ readTVar ts
   evalStateT sm s


webCommand :: (TVar Session) -> PlayerNumber -> StateT Session IO () -> RoutedNomyxServer ()
webCommand tm _ sm = liftRouteT $ lift $ do
   s <- atomically $ readTVar tm
   s' <- execStateT sm s
   atomically $ writeTVar tm s'


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
          [ ((if checked then (! A.checked "checked") else id) $
             input ! A.type_ "radio" ! A.id (toValue i) ! A.name (toValue nm) ! A.value (toValue val))
          , " ", H.label ! A.for (toValue i) $ toHtml lbl]

mainPage' :: String -> Html -> Html -> Bool -> RoutedNomyxServer Response
mainPage' title header body footer = do
   html <- mainPage title header body footer
   return $ toResponse html

mainPage :: String -> Html -> Html -> Bool -> RoutedNomyxServer Html
mainPage title header body footer = ok $ appTemplate' title header body footer

appTemplate' ::
       String -- ^ title
    -> Html   -- ^ extra tags to include in \<head\>
    -> Html   -- ^ contents to put inside \<body\>
    -> Bool   -- ^ include footer
    -> Html
appTemplate' title headers body footer = do
   H.head $ do
      H.title (string title)
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomyx.css"
      H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
      H.meta ! A.name "keywords" ! A.content "Nomyx, game, rules, Haskell, auto-reference"
      H.script ! A.type_ "text/JavaScript" ! A.src "/static/nomyx.js" $ ""
   H.body $ do
      H.div ! A.id "container" $ do
         H.div ! A.id "header" $ headers
         body
         when footer $ H.div ! A.id "footer" $ "Copyright Corentin Dupont 2012-2013"

appTemplate ::
    ( Monad m
    )
    => String -- ^ title
    -> Html  -- ^ extra tags to include in \<head\>
    -> Html    -- ^ contents to put inside \<body\>
    -> m Response
appTemplate title headers body = do
   return $ toResponse $ appTemplate' title headers body True


