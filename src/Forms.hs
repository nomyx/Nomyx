
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables, NamedFieldPuns, Rank2Types, DoAndIfThenElse, ExtendedDefaultRules#-}



module Forms where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map, label)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Reform
import Text.Reform.Blaze.String hiding (form)
import Happstack.Server
import Multi
import Language.Nomyx.Expression
import qualified Data.ByteString.Char8 as C
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Applicative
import Data.Text hiding (map, zip, concatMap)
import Data.List
import Data.Maybe
import Text.Reform.Happstack()
import qualified Text.Reform.Generalized as G
import qualified Text.Blaze.Html5  as H
default (Integer, Double, Data.Text.Text)

data LoginPass = LoginPass { login :: PlayerName,
                             password :: PlayerPassword}
                             deriving (Show, Eq)

data NewRuleForm = NewRuleForm { ruleName :: String,
                         ruleText :: String,
                         ruleCode :: String,
                         pn :: PlayerNumber }

data NewGameForm = NewGameForm { newGameName :: String,
                                 gamePn :: PlayerNumber }

type NomyxForm a = Form (ServerPartT IO) [Input] String Html () a

instance FormError String where
    type ErrorInputType String = [Input]
    commonFormError _ = "common error"


loginForm :: NomyxForm LoginPass
loginForm = pure LoginPass <*> label "Login: " ++> (inputText "") <*> label "    Password: " ++> inputPassword <++ label " "

inputChoiceForm :: String -> [String] -> String -> NomyxForm Int
inputChoiceForm title choices def = label (title ++ " ") ++> inputRadio' (zip [0..] choices) ((==) $ fromJust $ elemIndex def choices)

inputStringForm :: String -> NomyxForm String
inputStringForm title = label (title ++ " ") ++> inputText ""

uploadForm :: NomyxForm (FilePath, FilePath, ContentType)
uploadForm = inputFile

blazeResponse :: Html -> Response
blazeResponse html = toResponseBS (C.pack "text/html;charset=UTF-8") $ renderHtml html

blazeForm :: Html -> Text -> Html
blazeForm html link =
    form ! A.action (toValue link)
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
