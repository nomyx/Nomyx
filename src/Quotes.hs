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
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, QuasiQuotes, FlexibleInstances, GADTs #-}

module Quotes where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Interpreter hiding (get)
import Interpret


cr :: QuasiQuoter
cr = QuasiQuoter { quoteExp  = quoteRuleFunc,
                   quotePat  = undefined,
                   quoteType = undefined,
                   quoteDec  = undefined}

quoteRuleFunc :: String -> Q TH.Exp
quoteRuleFunc s = do
   res <- runIO $ runInterpreter $ do
      setImports importList
      typeOf s
   case res of
      Right "Nomex RuleResp" -> [| s |]
      Right "RuleFunc" -> [| s |]
      Right a -> fail $ "Rule doesn't typecheck: " ++ (show a)
      Left e -> fail $ show e




--cr2 :: QuasiQuoter
--cr2 = QuasiQuoter { quoteExp  = quoteRuleFunc2,
--                   quotePat  = undefined,
--                   quoteType = undefined,
--                   quoteDec  = undefined}
--
--quoteRuleFunc2 :: String -> Q TH.Exp
--quoteRuleFunc2 s = do
--   res <- runIO $ runInterpreter $ do
--      setImports importList
--      interpret s (as::RuleFunc)
--   case res of
--      Right f -> [| (s,f) |]
--      Left e -> fail $ show e


      --do --[| putParens s |]
--   s_0 <- newName "s"
--   return $ LamE [VarP s_0] (InfixE (Just (LitE (StringL "(")))
--                                    (VarE GHC.Base.++)
--                                    (Just (InfixE (Just (VarE s_0))
--
--
--putParens :: String -> String
--putParens s =  "(" ++ s ++ ",\"" ++ s ++ "\")"
--
--putParens' :: Q TH.Exp -> Q TH.Exp
--putParens' (LitE (StringL s)) = (LitE (StringL s))

--t :: (Int, String)
--t = $( [| 1 |] )
--RuleName -> RuleFunc -> RuleCode -> RuleNumber -> String -> Nomex ()

---- $( quoteRuleFunc2 "voidRule $ return ()")
--putParens :: String -> IO (String, RuleFunc)
--putParens s = do
--   res <- runIO $ runInterpreter $ do
--      setImports importList
--      interpret s (as::RuleFunc)
--   case res of
--      Right f -> return (s, f)
--      Left  e -> fail $ show e
