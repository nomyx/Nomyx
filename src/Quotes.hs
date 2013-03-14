-----------------------------------------------------------------------------
--
-- Module      :  Quotes
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  corentin.dupont@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, QuasiQuotes #-}

module Quotes where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Interpreter hiding (get)
import Interpret


cr :: QuasiQuoter
cr = QuasiQuoter { quoteExp = quoteRuleFunc,
                   quotePat = undefined,
                   quoteType = undefined,
                   quoteDec = undefined}

quoteRuleFunc :: String -> Q TH.Exp
quoteRuleFunc s = do
   res <- runIO $ runInterpreter $ do
      setImports importList
      typeOf s
   case res of
      Right "RuleFunc" -> [| s |]
      Right _ -> fail $ "Rule doesn't typecheck"
      Left e -> fail $ show e


-- addRuleParams'_ :: RuleName -> String -> RuleNumber -> String -> Exp ()
-- addRuleParams'_ name code number desc = addRuleParams_ name (VoidRule $ return ()) $( [|code|] ) number desc


