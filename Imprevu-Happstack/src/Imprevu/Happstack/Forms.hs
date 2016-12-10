{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Imprevu.Happstack.Forms where

import           Control.Monad.Extra                 (mapMaybeM)
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.String
import           Data.Text                           (Text, unpack)
import           Data.Typeable
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
import           Debug.NoTrace

default (Integer, Double, Data.Text.Text)

type BackLink = EventNumber -> Input -> Text

viewInput :: ClientNumber -> BackLink -> EventNumber -> [SomeSignal] -> ServerPartT IO (Maybe Html)
viewInput cn bl en ss = do
   ds <- mapMaybeM (viewInput' cn bl en) ss 
   return $ if null ds
      then Nothing
      else Just $ sequence_ ds
viewInput _ _ _ _ = return Nothing

viewInput' :: ClientNumber -> BackLink -> EventNumber -> SomeSignal -> ServerPartT IO (Maybe Html)
viewInput' me backlink en (SomeSignal (Signal s)) = do
  traceM $ "viewInput' " ++ (show s)
  case (cast s) of
   Just is@(Input i cn) | me == cn -> do
      lf  <- viewForm "user" $ inputForm' i
      let link = backlink en is
      traceM $ "viewInput' backlink=" ++ (unpack link)
      return $ Just $ tr $ td $ do
         --fromString title
          fromString " "
          blazeForm lf link ! A.id "InputForm"
   _ -> return Nothing

inputForm' :: InputField -> ImpForm InputData
inputForm' (Radio s choices)    = RadioData    <$> RB.label s ++> (RB.inputRadio choices (== 0)) <++ RB.label (" " :: String)
inputForm' (Text s)             = TextData     <$> RB.label s ++> (RB.inputText "") <++ label (" " :: String)
inputForm' (TextArea s)         = TextAreaData <$> RB.label s ++> (textarea 50 5  "") <++ label (" " :: String)
inputForm' (Button s)           = pure ButtonData   <$> RB.label s
inputForm' (Checkbox s choices) = CheckboxData <$> RB.label s ++> (inputCheckboxes choices $ const False) <++ label (" " :: String)


-- | a form result has been sent
newInput :: Input -> EventNumber -> TVar s -> UpdateSession s -> Text -> ServerPartT IO Response
newInput is@(Input i _) en tv updateSession bl = toResponse <$> do
   methodM POST
   r <- eitherForm environment "user" (inputForm' i)
   case r of
      (Right id) -> liftIO $ updateSession tv is id en
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

