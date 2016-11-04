
module Imprevu.Happstack.Test where

import Imprevu.Happstack.TestServer
import Imprevu
import Imprevu.Test.Test
import Imprevu.Test.TestMgt
import Control.Monad
import Control.Applicative

tests :: IO ()
tests = startTest $ do
   onEvent_ (inputText 1 "Test simple input: ") (putStrLn' . show)

   onEvent_ (True <$ inputButton 1 "Test sum input: True" <|> False <$ inputButton 1 "Test sum input: False") (putStrLn' . show)

   putStrLn' "Test product input:"
   onEvent_ ((,) <$> inputText 1 "Test product input: Left" <*> inputText 1 "Test product input: Right") (putStrLn' . show)

   putStrLn' "Test monadic input:"
   let e = do
       a <- inputText 1 "Test monadic input: Enter \"coco\""
       guard (a == "coco") >> inputText 1 "Hello coco"
   void $ onEvent_ e putStrLn'
   return ()
