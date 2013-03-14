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
import Data.List
import Data.Time.Clock
import Data.Lens
import Safe

playTests :: ServerHandle -> IO [(String, Bool)]
playTests sh = mapM (\(title, t, cond) -> (title,) <$> test sh t cond) tests

tests :: [(String, [TimedEvent], Multi -> Bool)]
tests = [("hello World",           noTime gameHelloWorld,         condHelloWorld),
         ("hello World 2 players", noTime gameHelloWorld2Players, condHelloWorld2Players),
         ("Partial Function 1",    noTime gamePartialFunction1,   condPartialFunction1),
         ("Partial Function 2",    gamePartialFunction2,          condPartialFunction2),
         ("Partial Function 3",    noTime gamePartialFunction3,   condPartialFunction3),
         ("Money transfer",        noTime gameMoneyTransfer,      condMoneyTransfer)]

dayZero :: UTCTime
dayZero = UTCTime (ModifiedJulianDay 0) 0

noTime :: [MultiEvent] -> [TimedEvent]
noTime mes = map (TE dayZero) mes

test :: ServerHandle -> [TimedEvent] -> (Multi -> Bool) -> IO Bool
test sh tes cond = do
   let m = defaultMulti (Settings (defaultLog "") sh defaultNetwork False) dayZero
   m' <- (loadTest tes m)
   (evaluate $ cond m') `catch` (\(e::SomeException) -> (putStrLn $ "Exception in test: " ++ show e) >> return False)


loadTest ::  [TimedEvent] -> Multi -> IO Multi
loadTest tes m = do
   m' <- execStateT (loadTimedEvents tes) m
   evaluate m'
   return m'

testException :: Multi -> SomeException -> IO Multi
testException m e = do
   putStrLn $ "Test Exception: " ++ show e
   return m

loadTestName :: Settings -> String -> IO Multi
loadTestName set testName = do
   let mt = find (\(name, _, _) -> name == testName) tests
   t <- getCurrentTime
   let m = defaultMulti set t
   case mt of
      Just (n, t, _) -> putStrLn ("Loading test game: " ++ n)  >> loadTest t m
      Nothing -> putStrLn "Test name not found" >> return m

printRule :: Q THS.Exp -> String
printRule r = unsafePerformIO $ do
   expr <- runQ r
   return $ pprint expr


onePlayerOneGame :: [MultiEvent]
onePlayerOneGame =
   [MultiNewPlayer (PlayerMulti {_mPlayerNumber = 1, _mPlayerName = "coco", _mPassword = "coco", _mMail = MailSettings {_mailTo = "", _mailNewInput = False, _mailNewRule = False, _mailNewOutput = False, _mailConfirmed = False}, _inGame = Nothing, _lastRule = Nothing}),
    MultiMailSettings (MailSettings {_mailTo = "c", _mailNewInput = True, _mailNewRule = True, _mailNewOutput = True, _mailConfirmed = True}) 1,
    MultiNewGame "test" (GameDesc "" "") 1,
    MultiJoinGame "test" 1]

twoPlayersOneGame :: [MultiEvent]
twoPlayersOneGame = onePlayerOneGame ++
   [MultiNewPlayer (PlayerMulti {_mPlayerNumber = 2, _mPlayerName = "bat", _mPassword = "bat", _mMail = MailSettings {_mailTo = "", _mailNewInput = False, _mailNewRule = False, _mailNewOutput = False, _mailConfirmed = False}, _inGame = Nothing, _lastRule = Nothing}),
    MultiMailSettings (MailSettings {_mailTo = "c", _mailNewInput = True, _mailNewRule = True, _mailNewOutput = True, _mailConfirmed = True}) 2,
    MultiJoinGame "test" 2]

submitRule ::  String -> [MultiEvent]
submitRule r = onePlayerOneGame ++
   [MultiSubmitRule (SubmitRule "" "" r) 1,
    MultiInputChoiceResult 3 0 1]



gameHelloWorld :: [MultiEvent]
gameHelloWorld = onePlayerOneGame ++ submitRule [cr|helloWorld|]

condHelloWorld :: Multi -> Bool
condHelloWorld m = (head $ outputs $ head $ games ^$ m) == (1, "hello, world!")

gameHelloWorld2Players :: [MultiEvent]
gameHelloWorld2Players = twoPlayersOneGame ++
   [MultiSubmitRule (SubmitRule "" "" [cr|helloWorld|]) 1,
   MultiInputChoiceResult 3 0 1,
   MultiInputChoiceResult 4 0 2]

condHelloWorld2Players :: Multi -> Bool
condHelloWorld2Players m = (head $ outputs $ head $ games ^$ m) == (1, "hello, world!")

partialFunction1 :: String
partialFunction1 = [cr|VoidRule $ readVar_ (V "toto1")|]

gamePartialFunction1 :: [MultiEvent]
gamePartialFunction1 = onePlayerOneGame ++ (submitRule partialFunction1)

-- rule has not been accepted due to exception
condPartialFunction1 :: Multi -> Bool
condPartialFunction1 m = (rStatus $ head $ rules $ head $ games ^$ m) == Pending &&
                         (take 5 $ snd $ head $ outputs $ head $ games ^$ m) == "Error"

partialFunction2 :: String
partialFunction2 = [cr|VoidRule $ do
   t <- getCurrentTime
   onEventOnce_ (Time $ addUTCTime 5 t) $ const $ readVar_ (V "toto2")|]

gamePartialFunction2 :: [TimedEvent]
gamePartialFunction2 = noTime onePlayerOneGame ++ (noTime $ submitRule partialFunction2) ++
    [TE (5 `addUTCTime` dayZero) $  (MultiTimeEvent $ 5 `addUTCTime` dayZero)]

-- rule has been accepted but exception happened later
condPartialFunction2 :: Multi -> Bool
condPartialFunction2 m = (rStatus $ headNote "cond failed" $ rules $ headNote "cond failed" $ games ^$ m) == Active &&
                         (take 5 $ snd $ headNote "cond failed" $ outputs $ headNote "cond failed" $ games ^$ m) == "Error"

--This rule blocks the game: the exception (variable not existing) is triggered during a "rule proposed" event,
--thus preventing to propose any new rule to the game.
partialFunction3 :: String
partialFunction3 = [cr|VoidRule $ do
   onEvent_ (RuleEv Proposed) $ const $ readVar_ (V "toto3")|]

gamePartialFunction3 :: [MultiEvent]
gamePartialFunction3 = onePlayerOneGame ++ (submitRule partialFunction3) ++ (submitRule [cr|nothing|])

-- rule has been accepted but no more rule can be proposed
condPartialFunction3 :: Multi -> Bool
condPartialFunction3 m = (length $ rules $ head $ games ^$ m) == 3

--Create bank accounts, win 100 Ecu on rule accepted (so 100 Ecu is won for each player), transfer 50 Ecu
gameMoneyTransfer :: [MultiEvent]
gameMoneyTransfer = twoPlayersOneGame ++
   [MultiSubmitRule (SubmitRule "" "" [cr|createBankAccount|]) 1,
   (MultiInputChoiceResult 5 0 1),
   (MultiInputChoiceResult 4 0 2),
   (MultiSubmitRule (SubmitRule "" "" [cr|winXEcuOnRuleAccepted 100|]) 1),
   (MultiInputChoiceResult 7 0 1),
   (MultiInputChoiceResult 6 0 2),
   (MultiSubmitRule (SubmitRule "" "" [cr|moneyTransfer|]) 2),
   (MultiInputChoiceResult 8 0 1),
   (MultiInputChoiceResult 7 0 2),
   (MultiInputChoiceResult 7 0 1),
   (MultiInputStringResult "Select Amount to transfert to player: 2" "50" 1)]

condMoneyTransfer :: Multi -> Bool
condMoneyTransfer m = (vName $ head $ variables $ head $ games ^$ m) == "Accounts"
