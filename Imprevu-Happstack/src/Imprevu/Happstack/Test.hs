{-# LANGUAGE ScopedTypeVariables  #-}

module Imprevu.Happstack.Test where

import Imprevu.Happstack.TestServer
import Imprevu
import Imprevu.Test.Test
import Imprevu.Test.TestMgt
import Control.Monad
import Control.Applicative

tests :: IO ()
tests = startTest $ do
   onEvent_ (inputText 1 "Enter text: ") (putStrLn' . show)

   onEvent_ (True <$ inputButton 2 "Click to respond True " <|> False <$ inputButton 2 "Click to respond False ") (putStrLn' . show)

   onEvent_ ((,) <$> inputText 3 "enter first value: " <*> inputText 3 "Enter second value: ") (putStrLn' . show)

   let e = do
       a <- inputText 4 "Enter text: "
       guard (a == "coco") >> inputText 4 "Hello coco! Enter additional text: "
   void $ onEvent_ e (\a -> putStrLn' a)
   
   onEvent_ (inputRadio 5 "enter first value: " [(True, "Yes"), (False, "No")]) (\a -> putStrLn' (if a then "Yes" else "No"))
   
   return ()
