{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Imprevu.Happstack.Forms where

import           Control.Monad.Extra                 (mapMaybeM)
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.String
import           Data.Text                           (Text)
import           Debug.Trace.Helpers                 (traceM)
import           Imprevu
import           Imprevu.Evaluation
import           Imprevu.Happstack.Types
import           Happstack.Server              as HS (Response, Method (..), methodM, seeOther, toResponse, ServerPartT)
import           Text.Blaze.Html5                    (ToMarkup, Html, toHtml, td, toValue, tr, (!), input)
import qualified Text.Blaze.Html5              as H  (form, label)
import           Text.Blaze.Html5.Attributes   as A  (id, type_, name, value, checked, for, action, method, enctype)
import           Text.Reform                         (eitherForm, viewForm, (<++), (++>), ErrorInputType, Form, FormError (..), FormInput)
import           Text.Reform.Blaze.String            (inputCheckboxes, label, textarea)
import qualified Text.Reform.Blaze.String      as RB
import           Text.Reform.Happstack               (environment)
import qualified Text.Reform.Generalized       as G
default (Integer, Double, Data.Text.Text)

type BackLink = EventNumber -> SignalAddress -> InputView -> ClientNumber -> Text

viewInput :: ClientNumber -> WebStateN n s -> BackLink -> EventInfoN n -> ServerPartT IO (Maybe Html)
viewInput cn (WebState tvs _ conf) bl ei@(EventInfo en _ _ SActive _) = do
   s <- liftIO $ atomically $ readTVar tvs
   ds <- mapMaybeM (viewInput' en cn bl) (getRemainingSignals ei (EvalEnv s conf))
   traceM $ "viewInput " ++ (show $ length ds)
   return $ if null ds
      then Nothing
      else Just $ sequence_ ds
viewInput _ _ _ _ = return Nothing

viewInput' :: EventNumber -> ClientNumber -> BackLink -> (SignalAddress, SomeSignal) -> ServerPartT IO (Maybe Html)
viewInput' en me bl (sa, (SomeSignal (InputS s cn)))
  | me == cn = do
     traceM $ "viewInput' " ++ (show s)
     let iv = viewSignal s
     lf  <- viewForm "user" $ inputForm' iv
     let link = bl en sa iv me
     return $ Just $ tr $ td $ do
          --fromString title
          fromString " "
          blazeForm lf link ! A.id "InputForm"
viewInput' _ _ _ _ = return Nothing

inputForm' :: InputView -> ImpForm InputDataView
inputForm' (RadioField s choices)    = RadioData    <$> RB.label s ++> (RB.inputRadio choices (== 0)) <++ RB.label (" " :: String)
inputForm' (TextField s)             = TextData     <$> RB.label s ++> (RB.inputText "") <++ label (" " :: String)
inputForm' (TextAreaField s)         = TextAreaData <$> RB.label s ++> (textarea 50 5  "") <++ label (" " :: String)
inputForm' (ButtonField s)           = pure ButtonData   <$> RB.label s
inputForm' (CheckboxField s choices) = CheckboxData <$> RB.label s ++> (inputCheckboxes choices $ const False) <++ label (" " :: String)


-- | a form result has been sent
newInput :: EventNumber -> SignalAddress -> InputView -> WebStateN n s -> ClientNumber -> Text -> ServerPartT IO Response
newInput en sa ff (WebState tv updateSession _) pn bl = toResponse <$> do
   methodM POST
   r <- eitherForm environment "user" (inputForm' ff)
   case r of
      (Right c) -> liftIO $ updateSession tv $ InputResult en sa ff c pn
      (Left _) ->  liftIO $ putStrLn "cannot retrieve form data"
   seeOther bl "Redirecting..."

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

