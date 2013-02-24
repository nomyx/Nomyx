-----------------------------------------------------------------------------
--
-- Module      :  Test
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
{-# LANGUAGE TupleSections, ScopedTypeVariables, TemplateHaskell, GADTs, QuasiQuotes #-}

module Test where

import Prelude hiding (catch)
import Types
import Data.Time hiding (getCurrentTime)
import Control.Monad.State
import Serialize
import Language.Haskell.Interpreter.Server (ServerHandle)
import Language.Nomyx.Expression
import Control.Applicative
import Control.Exception
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS
import System.IO.Unsafe
import Quotes

playTests :: ServerHandle -> IO [(String, Bool)]
playTests sh = mapM (\(title, t, cond) -> (title,) <$> test sh t cond) tests

tests :: [(String, [TimedEvent], Multi -> Bool)]
tests = [("hello World", noTime gameHelloWorld, condHelloWorld),
         ("Partial Function 1", noTime gamePartialFunction1, condPartialFunction1),
         ("Partial Function 2", gamePartialFunction2, condPartialFunction2),
         ("Partial Function 3", noTime gamePartialFunction3, condPartialFunction3)]

dayZero :: UTCTime
dayZero = UTCTime (ModifiedJulianDay 0) 0

noTime :: [MultiEvent] -> [TimedEvent]
noTime mes = map (TE dayZero) mes

test :: ServerHandle -> [TimedEvent] -> (Multi -> Bool) -> IO Bool
test sh tes cond = do
   let m = defaultMulti sh "" defaultNetwork dayZero
   m' <- (test' tes m) `catch` (testException m)
   putStrLn $ show m'
   evaluate m'
   return $ cond m'

test' ::  [TimedEvent] -> Multi -> IO Multi
test' tes m = execStateT (loadTimedEvents tes) m


testException :: Multi -> SomeException -> IO Multi
testException m e = do
   putStrLn $ "Test Exception: " ++ show e
   return m

printRule :: Q THS.Exp -> String
printRule r = unsafePerformIO $ do
   expr <- runQ r
   return $ pprint expr


onePlayerOneGame :: [MultiEvent]
onePlayerOneGame =
   [MultiNewPlayer (PlayerMulti {mPlayerNumber = 1, mPlayerName = "coco", mPassword = "coco", mMail = MailSettings {mailTo = "", mailNewInput = False, mailNewRule = False, mailNewOutput = False, mailConfirmed = False}, inGame = Nothing, lastRule = Nothing}),
    MultiMailSettings (MailSettings {mailTo = "c", mailNewInput = True, mailNewRule = True, mailNewOutput = True, mailConfirmed = True}) 1,
    MultiNewGame "test" "" 1,
    MultiJoinGame "test" 1]


submitRule ::  String -> [MultiEvent]
submitRule r = onePlayerOneGame ++
   [MultiSubmitRule (SubmitRule "" "" r) 1,
    MultiInputChoiceResult 4 0 1]



gameHelloWorld :: [MultiEvent]
gameHelloWorld = onePlayerOneGame ++ submitRule [cr|nothing|]

condHelloWorld :: Multi -> Bool
condHelloWorld m = (head $ outputs $ head $ games m) == (1, "hello, world!")

partialFunction1 :: String
partialFunction1 = [cr|VoidRule $ readVar_ (V "toto1")|]

gamePartialFunction1 :: [MultiEvent]
gamePartialFunction1 = onePlayerOneGame ++ (submitRule partialFunction1)

condPartialFunction1 :: Multi -> Bool
condPartialFunction1 m = (length $ rules $ head $ games m) == 2

partialFunction2 :: String
partialFunction2 = [cr|VoidRule $ do
   t <- getCurrentTime
   onEventOnce_ (Time $ addUTCTime 5 t) $ const $ readVar_ (V "toto2")|]

gamePartialFunction2 :: [TimedEvent]
gamePartialFunction2 = noTime onePlayerOneGame ++ (noTime $ submitRule partialFunction2) ++
    [TE (5 `addUTCTime` dayZero) $  (MultiTimeEvent $ 5 `addUTCTime` dayZero)]

condPartialFunction2 :: Multi -> Bool
condPartialFunction2 m = (length $ rules $ head $ games m) == 2 &&
                         (take 5 $ snd $ head $ outputs $ head $ games m) == "Error"

--This rule blocks the game: the exception (variable not existing) is triggered during a "rule proposed" event,
--thus preventing to propose any new rule to the game.
partialFunction3 :: String
partialFunction3 = [cr|VoidRule $ do
   onEvent_ (RuleEv Proposed) $ const $ readVar_ (V "toto3")|]

gamePartialFunction3 :: [MultiEvent]
gamePartialFunction3 = onePlayerOneGame ++ (submitRule partialFunction3) ++ (submitRule [cr|nothing|])

condPartialFunction3 :: Multi -> Bool
condPartialFunction3 m = (length $ rules $ head $ games m) == 3
