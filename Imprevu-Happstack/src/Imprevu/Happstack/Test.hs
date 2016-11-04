
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

   onEvent_ (True <$ inputButton 2 "Test sum input: True " <|> False <$ inputButton 2 "Test sum input: False ") (putStrLn' . show)

   onEvent_ ((,) <$> inputText 3 "Test product input: Left " <*> inputText 3 "Test product input: Right ") (putStrLn' . show)

   let e = do
       a <- inputText 4 "Test monadic input: Enter \"coco\" "
       guard (a == "coco") >> inputText 4 "Hello coco "
   void $ onEvent_ e putStrLn'
   return ()
