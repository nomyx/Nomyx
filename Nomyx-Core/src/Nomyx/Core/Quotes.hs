
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Nomyx.Core.Quotes where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Interpreter hiding (get)
import Nomyx.Core.Engine.Context


-- This quasi quoter allows to type check a string as a Nomyx rule.
-- this gives additionnal safety at compile time.
#ifdef NO_INTERPRET_QUOTES
cr :: QuasiQuoter
cr = QuasiQuoter { quoteExp  = \s -> [| s |],
                   quotePat  = undefined,
                   quoteType = undefined,
                   quoteDec  = undefined}
#else
cr :: QuasiQuoter
cr = QuasiQuoter { quoteExp  = quoteRuleFunc,
                   quotePat  = undefined,
                   quoteType = undefined,
                   quoteDec  = undefined}
#endif

quoteRuleFunc :: String -> Q TH.Exp
quoteRuleFunc s = do
   res <- runIO $ runInterpreter $ do
      setImports unQualImports
      typeOf s
   case res of
      Right "Exp Effect ()"  -> [| s |]
      Right "Nomex ()"       -> [| s |]
      Right "Exp 'Effect ()" -> [| s |]
      Right "Rule"           -> [| s |]
      Right a -> fail $ "Rule doesn't typecheck: " ++ show a
      Left  e -> fail $ show e



