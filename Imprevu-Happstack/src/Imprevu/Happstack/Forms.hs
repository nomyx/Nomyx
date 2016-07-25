{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ExistentialQuantification    #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Imprevu.Happstack.Forms where

import Imprevu.Internal.Event
import Imprevu.Internal.EventEval
import Imprevu.Internal.InputEval
--import Imprevu.Events
import Imprevu.Inputs
import Imprevu.Happstack.Types
import Control.Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Applicative
import Control.Concurrent.STM
import Data.Monoid
import Data.Maybe
import Data.Typeable
import Data.String
import Data.Data
import           Control.Monad.State
import qualified Happstack.Server    as HS (Input)
import Happstack.Server              as HS (Response, ServerPartT, Method (..), methodM,
                                              ok, seeOther, toResponse)
import           Text.Blaze.Html5            (ToMarkup, Html, toHtml, a, br, div, h3, h4,
                                              pre, table, td, toValue, tr, (!), input)
import qualified Text.Blaze.Html5    as H    (form, label)
import           Text.Blaze.Html5.Attributes as A (id, href, type_, name, value, checked, for, action, method, enctype)
import           Text.Reform                 (eitherForm, viewForm, (<++), CommonFormError, ErrorInputType,
                                                Form, FormError (..), FormInput)
import           Text.Reform.Blaze.String    (inputCheckboxes, label, textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT, RouteT)
import qualified Text.Reform.Generalized             as G
import           Web.Routes.Base
import           Web.Routes.Happstack                ()
import           Web.Routes.RouteT
import           Web.Routes.PathInfo
import  Data.Text                           (Text)
import Unsafe.Coerce
default (Integer, Double, Data.Text.Text)


viewInput :: (HasEvents n s) => EventInfo n -> RoutedServer n s (Maybe Html)
viewInput ei@(EventInfo en _ _ SActive _) = do
   (WebState tvs _ f g) <- get
   s <- liftIO $ atomically $ readTVar tvs
   ds <- mapMaybeM (viewInput' en) (getRemainingSignals ei (EvalEnv s f g))
   return $ if null ds
      then Nothing
      else Just $ sequence_ ds
viewInput _ = return Nothing

viewInput' :: EventNumber -> (SignalAddress, SomeSignal) -> RoutedServer n s (Maybe Html)
viewInput' en (fa, ss@(SomeSignal a)) = do
    if (toConstr a) == (toConstr ((Input "" Button) :: Input ()))
      then do
        let (ev :: Input ()) = unsafeCoerce a
        lf  <- liftRouteT $ lift $ viewForm "user" $ inputForm ev
        let link = showRelURL (DoInput en fa (fromJust $ getFormField' ev))
        return $ Just $ tr $ td $ do
--          fromString title
          fromString " "
          blazeForm lf link ! A.id "InputForm"
      else return Nothing
viewInput' _ _ = return Nothing


--- TODO: merge SomeSignal and FormField...
inputForm :: Imprevu.Inputs.Input a -> ImpForm InputData
inputForm ((Input _ (Radio choices)))    = RadioData    <$> (reformInputRadio' (zip [0..] (snd <$> choices)) (== 0) <++ label (" " :: String))
inputForm ((Input _ Text))               = TextData     <$> RB.inputText "" <++ label (" " :: String)
inputForm ((Input _ TextArea))           = TextAreaData <$> textarea 50 5  "" <++ label (" " :: String)
inputForm ((Input _ Button))             = pure ButtonData
inputForm ((Input _ (Checkbox choices))) = CheckboxData <$> inputCheckboxes (zip [0..] (snd <$> choices)) (const False) <++ label (" " :: String)
inputForm _ = error "Not an input form"


inputForm' :: FormField -> ImpForm InputData
inputForm' (RadioField _ choices)    = RadioData    <$> (RB.inputRadio choices (== 0)) <++ RB.label (" " :: String)
inputForm' (TextField _)             = TextData     <$> RB.inputText "" <++ label (" " :: String)
inputForm' (TextAreaField _)         = TextAreaData <$> textarea 50 5  "" <++ label (" " :: String)
inputForm' (ButtonField _)           = pure ButtonData
inputForm' (CheckboxField _ choices) = CheckboxData <$> inputCheckboxes choices (const False) <++ label (" " :: String)

getFormField' :: Input a -> Maybe FormField
getFormField' (Input _ (Radio choices)) = Just $ RadioField    "" (zip [0..] (snd <$> choices))
getFormField' (Input _ Text)          = Just $ TextField     ""
getFormField' (Input _ TextArea)      = Just $ TextAreaField ""
getFormField' (Input _ Button)        = Just $ ButtonField   ""
getFormField' (Input _ (Checkbox choices)) = Just $ CheckboxField "" (zip [1..] (snd <$> choices))
--getFormField _ = Nothing

-- | a form result has been sent
newInput :: EventNumber -> SignalAddress -> FormField -> RoutedServer n s Response
newInput en fa ft = toResponse <$> do
   methodM POST
   (WebState tv updateSession _ _) <- get
   r <- liftRouteT $ lift $ eitherForm environment "user" (inputForm' ft)
   case r of
      (Right c) -> liftIO $ updateSession tv $ InputResult en fa ft c
      (Left _) ->  liftIO $ putStrLn "cannot retrieve form data"
   seeOther (showRelURL $ Main) "Redirecting..."

showRelURL :: Command -> Text
showRelURL c = "/Nomyx" <> (toPathInfo c)

-- | Create a group of radio elements without BR between elements
reformInputRadio' :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
              [(a, lbl)]  -- ^ value, label, initially checked
           -> (a -> Bool) -- ^ isDefault
           -> Form m input error Html () a
reformInputRadio' choices isDefault =
   G.inputChoice isDefault choices mkRadios
   where
      mkRadios nm choices' = mconcat $ concatMap (mkRadio nm) choices'
      mkRadio nm (i, val, lbl, checked) =
         [ (if checked then (! A.checked "checked") else Prelude.id) $ input ! A.type_ "radio" ! A.id (toValue i) ! A.name (toValue nm) ! A.value (toValue val)
         , " ", H.label ! A.for (toValue i) $ toHtml lbl]


blazeForm :: Html -> Text -> Html
blazeForm html link =
    H.form ! A.action (toValue link)
         ! A.method "POST"
         ! A.enctype "multipart/form-data" $
            do html
               input ! A.type_ "submit" ! A.value "Submit"

