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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Test module
module Test where

import Prelude
import Types
import Control.Monad.State
import Multi
import Session
import Utils
import Language.Haskell.Interpreter.Server (ServerHandle)
import Language.Nomyx hiding (getCurrentTime)
import Language.Nomyx.Engine
import Control.Applicative
import Control.Exception as E
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS hiding (lift)
import System.IO.Unsafe
import Quotes
import Data.Lens
import Data.List
import Safe
import Data.Acid.Memory
import Happstack.Auth.Core.Auth (initialAuthState)
import Happstack.Auth.Core.Profile (initialProfileState)
import qualified Language.Nomyx.Engine as G
import Control.Arrow ((>>>))
import Data.Time hiding (getCurrentTime)
import System.IO.Temp
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

playTests :: FilePath -> ServerHandle -> IO [(String, Bool)]
playTests dataDir sh = do
   tp <- testProfiles
   dir <- createTempDirectory "/tmp" "Nomyx"
   createDirectoryIfMissing True $ dir </> uploadDir
   let session = Session sh (defaultMulti (Settings {_net = defaultNetwork, _sendMails = False, _adminPassword = "", _saveDir = dir, _dataDir = dataDir, _sourceDir = ""})) tp
   mapM (\(title, t, cond) -> (title,) <$> test session t cond) tests

-- | test list.
-- each test can be loaded individually in Nomyx with the command line:
-- Nomyx -l <"test name">
tests :: [(String, StateT Session IO (), Multi -> Bool)]
tests = [("hello World",           gameHelloWorld,         condHelloWorld),
         ("hello World 2 players", gameHelloWorld2Players, condHelloWorld2Players),
         ("Money transfer",        gameMoneyTransfer,      condMoneyTransfer),
         ("Partial Function 1",    gamePartialFunction1,   condPartialFunction1),
         ("Partial Function 2",    gamePartialFunction2,   condPartialFunction2),
         ("Partial Function 3",    gamePartialFunction3,   condPartialFunction3),
         ("Infinite loop 1",       gameLoop1,              condLoop1),
         ("Infinite loop 2",       gameLoop2,              condLoop2),
         ("Test file 1",           testFile1,              condFile1),
         ("Test file 2",           testFile2,              condFile2),
         ("load file twice",       testFileTwice,          condFileTwice),
         ("load file twice 2",     testFileTwice',         condFileTwice'),
         ("load file unsafe",      testFileUnsafeIO,       condFileUnsafeIO)]




test :: Session -> StateT Session IO () -> (Multi -> Bool) -> IO Bool
test session tes cond = do
   m' <- loadTest tes session
   (evaluate $ cond m') `E.catch` (\(e::SomeException) -> (putStrLn $ "Exception in test: " ++ show e) >> return False)

--Loads a test
loadTest ::  StateT Session IO () -> Session -> IO Multi
loadTest tes s = do
   ms <- updateSession' s tes --version with no watchdog: ms <- Just <$> execStateT tes s
   return $ _multi $ case ms of
      Just s' -> s'
      Nothing -> s

testException :: Multi -> SomeException -> IO Multi
testException m e = do
   putStrLn $ "Test Exception: " ++ show e
   return m

loadTestName :: Settings -> String -> ServerHandle -> IO Multi
loadTestName set testName sh = do
   let mt = find (\(name, _, _) -> name == testName) tests
   tp <- testProfiles
   let s = Session sh (defaultMulti set) tp
   case mt of
      Just (n, t, _) -> putStrLn ("Loading test game: " ++ n)  >> loadTest t s
      Nothing -> do
         putStrLn "Test name not found"
         return $ _multi s

testProfiles :: IO Profiles
testProfiles = do
   ias  <- openMemoryState initialAuthState
   ips  <- openMemoryState initialProfileState
   ipds <- openMemoryState initialProfileDataState
   return $ Profiles ias ips ipds

printRule :: Q THS.Exp -> String
printRule r = unsafePerformIO $ do
   expr <- runQ r
   return $ pprint expr

onePlayerOneGame :: StateT Session IO ()
onePlayerOneGame = do
   newPlayer 1 (PlayerSettings {_pPlayerName = "Player 1", _mail = "", _mailNewInput = False, _mailNewRule = False, _mailNewOutput = False, _mailConfirmed = False})
   newGame "test" (GameDesc "" "") 1
   joinGame "test" 1
   viewGamePlayer "test" 1

twoPlayersOneGame :: StateT Session IO ()
twoPlayersOneGame = do
   onePlayerOneGame
   newPlayer 2 (PlayerSettings {_pPlayerName = "Player 2", _mail = "", _mailNewInput = False, _mailNewRule = False, _mailNewOutput = False, _mailConfirmed = False})
   joinGame "test" 2
   viewGamePlayer "test" 2

submitR :: String -> StateT Session IO ()
submitR r = do
   onePlayerOneGame
   sh <- access sh
   submitRule (SubmitRule "" "" r) 1 "test" sh
   inputAllRadios 0 1

testFile' :: FilePath -> FilePath -> String -> StateT Session IO Bool
testFile' path name func = do
   sh <- access sh
   set <- access (multi >>> mSettings)
   res <- inputUpload 1 (getTestDir set </> path) name sh
   submitRule (SubmitRule "" "" func) 1 "test" sh
   inputAllRadios 0 1
   return res

testFile :: FilePath -> String -> StateT Session IO Bool
testFile name function = testFile' name name function

-- * Tests

-- ** Standard tests

gameHelloWorld :: StateT Session IO ()
gameHelloWorld = submitR [cr|helloWorld|]

condHelloWorld :: Multi -> Bool
condHelloWorld m = isOutput' "hello, world!" m

gameHelloWorld2Players :: StateT Session IO ()
gameHelloWorld2Players = do
   twoPlayersOneGame
   sh <- access sh
   submitRule (SubmitRule "" "" [cr|helloWorld|]) 1 "test" sh
   inputAllRadios 0 1
   inputAllRadios 0 2

condHelloWorld2Players :: Multi -> Bool
condHelloWorld2Players m = isOutput' "hello, world!" m

--Create bank accounts, win 100 Ecu on rule accepted (so 100 Ecu is won for each player), transfer 50 Ecu
--TODO fix the text input
gameMoneyTransfer :: StateT Session IO ()
gameMoneyTransfer = do
   sh <- access sh
   twoPlayersOneGame
   submitRule (SubmitRule "" "" [cr|createBankAccount|]) 1 "test" sh
   submitRule (SubmitRule "" "" [cr|winXEcuOnRuleAccepted 100|]) 1 "test" sh
   submitRule (SubmitRule "" "" [cr|moneyTransfer|]) 2 "test" sh
   inputAllRadios 0 1
   inputAllRadios 0 2
   inputAllTexts "50" 1

condMoneyTransfer :: Multi -> Bool
condMoneyTransfer m = (_vName $ head $ _variables $ firstGame m) == "Accounts"

-- ** Partial functions

partialFunction1 :: String
partialFunction1 = [cr|ruleFunc $ readMsgVar_ (msgVar "toto1" :: MsgVar String)|]

gamePartialFunction1 :: StateT Session IO ()
gamePartialFunction1 = submitR partialFunction1

-- rule has not been accepted due to exception
condPartialFunction1 :: Multi -> Bool
condPartialFunction1 m = (_rStatus $ head $ _rules $ firstGame m) == Active &&
                         (take 5 $ _lMsg $ head $ _logs $ firstGame m) == "Error"

partialFunction2 :: String
partialFunction2 = [cr|ruleFunc $ do
   t <- liftEffect getCurrentTime
   onEventOnce (Time $ addUTCTime 5 t) $ const $ readMsgVar_ (msgVar "toto2")|]

gamePartialFunction2 :: StateT Session IO ()
gamePartialFunction2 = do
   onePlayerOneGame
   submitR partialFunction2
   gs <- (access $ multi >>> games)
   let now = _currentTime $ G._game (head gs)
   focus multi $ triggerTimeEvent (5 `addUTCTime` now)

-- rule has been accepted but exception happened later
condPartialFunction2 :: Multi -> Bool
condPartialFunction2 m = (_rStatus $ headNote "cond1 failed" $ _rules $ firstGame m) == Active &&
                         (take 5 $ _lMsg $ headNote "cond3 failed" $ _logs $ firstGame m) == "Error"

partialFunction3 :: String
partialFunction3 = [cr|ruleFunc $ onEvent_ (RuleEv Proposed) $ const $ readMsgVar_ (msgVar "toto3")|]

gamePartialFunction3 :: StateT Session IO ()
gamePartialFunction3 = do
   submitR partialFunction3
   submitR [cr|nothing|]

-- rule has been accepted and also next one
condPartialFunction3 :: Multi -> Bool
condPartialFunction3 m = (length $ _rules $ firstGame m) == 4

-- * Malicious codes

--an infinite loop: it should be interrupted by the watchdog
gameLoop1 :: StateT Session IO ()
gameLoop1 = submitR [cr|
   ruleFunc $ do
      let x = 1 + x
      outputAll_ $ show x
   |]

--the game created should be withdrawn
condLoop1 :: Multi -> Bool
condLoop1 m = (length $ _games m) == 0

--an infinite loop: it should be interrupted by the watchdog
gameLoop2 :: StateT Session IO ()
gameLoop2 = submitR [cr|ruleFunc $ outputAll_ $ show $ repeat 1|]

--the game created should be withdrawn
condLoop2 :: Multi -> Bool
condLoop2 m = (length $ _games m) == 0

-- ** File loading

--standard module
testFile1 :: StateT Session IO ()
testFile1 = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "myRule"

condFile1 :: Multi -> Bool
condFile1 m = (length $ _rules $ firstGame m) == 3

--standard module, call with namespace
testFile2 :: StateT Session IO ()
testFile2 = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "SimpleModule.myRule"

condFile2 :: Multi -> Bool
condFile2 m = (length $ _rules $ firstGame m) == 3

--loading two modules with the same name is forbidden
testFileTwice :: StateT Session IO ()
testFileTwice = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "SimpleModule.myRule"
   void $ testFile' "more/SimpleModule.hs" "SimpleModule.hs" "SimpleModule.myRule2"


condFileTwice :: Multi -> Bool
condFileTwice m = (length $ _rules $ firstGame m) == 3

--but having the same function name in different modules is OK
testFileTwice' :: StateT Session IO ()
testFileTwice' = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "SimpleModule.myRule"
   void $ testFile "SimpleModule2.hs" "SimpleModule2.myRule"

condFileTwice' :: Multi -> Bool
condFileTwice' m = (length $ _rules $ firstGame m) == 4

--security: no unsafe module imports
testFileUnsafeIO :: StateT Session IO ()
testFileUnsafeIO = do
   onePlayerOneGame
   void $ testFile "UnsafeIO.hs" "UnsafeIO.myRule"

condFileUnsafeIO :: Multi -> Bool
condFileUnsafeIO m = (length $ _rules $ firstGame m) == 2

-- * Helpers

--True if the string in parameter is among the outputs
isOutput' :: String -> Multi -> Bool
isOutput' s m = any ((isOutput s) . _game) (_games m)

-- select first choice for all radio buttons
inputAllRadios :: Int -> PlayerNumber -> StateT Session IO ()
inputAllRadios choice pn = do
   s <- get
   let evs = evalState getChoiceEvents (_game $ head $ _games $ _multi s)
   mapM_ (\en -> inputResult pn en (URadioData choice) "test") evs

-- input text for all text fields
inputAllTexts :: String -> PlayerNumber -> StateT Session IO ()
inputAllTexts a pn = do
   s <- get
   let evs = evalState getTextEvents (_game $ head $ _games $ _multi s)
   mapM_ (\en -> inputResult pn en (UTextData a) "test") evs

firstGame :: Multi -> Game
firstGame = G._game . head . _games
