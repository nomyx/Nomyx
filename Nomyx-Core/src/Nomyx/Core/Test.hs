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
module Nomyx.Core.Test where

import Language.Haskell.Interpreter.Server (ServerHandle)
import Language.Nomyx hiding (getCurrentTime)
import Control.Applicative
import Control.Monad.State
import Control.Exception as E
import Control.Arrow ((>>>))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS hiding (lift)
import System.IO.Unsafe
import Data.Lens
import Data.List
import Data.Maybe
import Data.Acid.Memory
import Data.Time hiding (getCurrentTime)
import Happstack.Auth.Core.Auth (initialAuthState)
import Happstack.Auth.Core.Profile (initialProfileState)
import Paths_Nomyx_Core as PNC
import System.IO.Temp
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Nomyx.Core.Types
import Nomyx.Core.Multi
import Nomyx.Core.Session
import Nomyx.Core.Utils
import Nomyx.Core.Profile
import Nomyx.Core.Quotes
import Nomyx.Core.Engine
import Nomyx.Core.Engine.Types
import qualified Nomyx.Core.Engine as G

playTests :: FilePath -> ServerHandle -> Maybe String -> IO [(String, Bool)]
playTests saveDir sh mTestName = do
   tests <- case mTestName of
      Just testName -> do
         let tsts = fatalTests ++ regularTests
         return $ maybeToList $ find (\(name, _, _) -> name == testName) tsts
      Nothing -> return regularTests
   tp <- testProfiles
   dir <- createTempDirectory "/tmp" "Nomyx"
   createDirectoryIfMissing True $ dir </> uploadDir
   let session = Session sh (defaultMulti Settings {_net = defaultNetwork, _sendMails = False, _adminPassword = "", _saveDir = saveDir, _webDir = "", _sourceDir = ""}) tp
   mapM (\(title, t, cond) -> (title,) <$> test title session t cond) tests

-- | test list.
-- each test can be loaded individually in Nomyx with the command line:
-- Nomyx -l <"test name">
regularTests :: [(String, StateT Session IO (), Multi -> Bool)]
regularTests = [("hello World",           gameHelloWorld,         condHelloWorld),
         ("hello World 2 players", gameHelloWorld2Players, condHelloWorld2Players),
         ("Money transfer",        gameMoneyTransfer,      condMoneyTransfer),
         ("Partial Function 1",    gamePartialFunction1,   condPartialFunction),
         ("Partial Function 2",    gamePartialFunction2,   condPartialFunction),
         ("Partial Function 3",    gamePartialFunction3,   condPartialFunction3),
         ("Test file 1",           testFile1,              condNRules 3),
         ("Test file 2",           testFile2,              condNRules 3),
         ("load file twice",       testFileTwice,          condNRules 3),
         ("load file twice 2",     testFileTwice',         condNRules 4),
         ("load file unsafe",      testFileUnsafeIO,       condNRules 2)] ++
         map (\i -> ("Loop" ++ show i,      loops !! (i-1),      condNoGame))   [1..(length loops)] ++
         map (\i -> ("Forbidden" ++ show i, forbiddens !! (i-1), condNRules 2)) [1..(length forbiddens)]

-- Those tests should make the game die immediately because of security problem (it will be re-launched)
fatalTests :: [(String, StateT Session IO (), Multi -> Bool)]
fatalTests = [("Timeout type check", gameBadTypeCheck, const True)]


test :: String -> Session -> StateT Session IO () -> (Multi -> Bool) -> IO Bool
test title session tes cond = do
   putStrLn $ "\nPlaying test: " ++ title
   m' <- loadTest tes session
   (evaluate $ cond m') `E.catch` (\(e::SomeException) -> (putStrLn $ "Exception in test: " ++ show e) >> return False)

--Loads a test
loadTest ::  StateT Session IO () -> Session -> IO Multi
loadTest tes s = do
   ms <- evalWithWatchdog s (evalSession tes) --version with no watchdog: ms <- Just <$> execStateT tes s
   case ms of
      Just s' -> return $ _multi s'
      Nothing -> do
         putStrLn "thread timed out, updateSession discarded"
         return $ _multi s

testException :: Multi -> SomeException -> IO Multi
testException m e = do
   putStrLn $ "Test Exception: " ++ show e
   return m

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
   newPlayer 1 PlayerSettings {_pPlayerName = "Player 1", _mail = "", _mailNewInput = False, _mailNewRule = False, _mailNewOutput = False, _mailConfirmed = False}
   newGame "test" (GameDesc "" "") 1 True
   joinGame "test" 1
   viewGamePlayer "test" 1

twoPlayersOneGame :: StateT Session IO ()
twoPlayersOneGame = do
   onePlayerOneGame
   newPlayer 2 PlayerSettings {_pPlayerName = "Player 2", _mail = "", _mailNewInput = False, _mailNewRule = False, _mailNewOutput = False, _mailConfirmed = False}
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
   dataDir <- lift PNC.getDataDir
   res <- inputUpload 1 (dataDir </> testDir </> path) name sh
   submitRule (SubmitRule "" "" func) 1 "test" sh
   inputAllRadios 0 1
   return res

testFile :: FilePath -> String -> StateT Session IO Bool
testFile name = testFile' name name

-- * Tests

-- ** Standard tests

gameHelloWorld :: StateT Session IO ()
gameHelloWorld = submitR [cr|helloWorld|]

condHelloWorld :: Multi -> Bool
condHelloWorld = isOutput' "hello, world!"

gameHelloWorld2Players :: StateT Session IO ()
gameHelloWorld2Players = do
   twoPlayersOneGame
   sh <- access sh
   submitRule (SubmitRule "" "" [cr|helloWorld|]) 1 "test" sh
   inputAllRadios 0 1
   inputAllRadios 0 2

condHelloWorld2Players :: Multi -> Bool
condHelloWorld2Players = isOutput' "hello, world!"

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
partialFunction1 = [cr|void $ readMsgVar_ (msgVar "toto1" :: MsgVar String)|]

partialFunction2 :: String
partialFunction2 = [cr|void $ do
   t <- liftEffect getCurrentTime
   onEventOnce (return $ timeEvent $ addUTCTime 5 t) $ const $ readMsgVar_ (msgVar "toto2")|]

gamePartialFunction1 :: StateT Session IO ()
gamePartialFunction1 = submitR partialFunction1

gamePartialFunction2 :: StateT Session IO ()
gamePartialFunction2 = do
   onePlayerOneGame
   submitR partialFunction2
   gs <- (access $ multi >>> gameInfos)
   let now = _currentTime $ G._game $ _loggedGame $ head gs
   focus multi $ triggerTimeEvent (5 `addUTCTime` now)


-- rule has not been accepted due to exception
condPartialFunction :: Multi -> Bool
condPartialFunction m = (_rStatus $ head $ _rules $ firstGame m) == Active &&
                        (take 5 $ _lMsg $ head $ _logs $ firstGame m) == "Error"


partialFunction3 :: String
partialFunction3 = [cr|void $ onEvent_ (return $ ruleEvent Proposed) $ const $ readMsgVar_ (msgVar "toto3")|]

gamePartialFunction3 :: StateT Session IO ()
gamePartialFunction3 = do
   submitR partialFunction3
   submitR [cr|nothing|]

-- rule has been accepted and also next one
condPartialFunction3 :: Multi -> Bool
condPartialFunction3 m = (length $ _rules $ firstGame m) == 4

-- * Malicious codes



--infinite loops: they should be interrupted by the watchdog & resource limits
loops, forbiddens :: [StateT Session IO ()]
loops = [loop1, loop2, loop3, loop4, loop5, loop6, stackOverflow, outputLimit]
forbiddens = [forbid1, forbid2, forbid3, forbid4, forbid5, forbid6]

loop1  = submitR [cr| let x :: Int; x = x                              in showRule x |]
loop2  = submitR [cr| let f :: Int -> Int; f y = f 1                   in showRule (f 1) |]
loop3  = submitR [cr| let x = x + 1                                    in showRule x |]
loop4  = submitR [cr| let f :: Int -> Int; f x = f $! (x+1)            in showRule (f 0) |]
--test stack overflow limits
loop5  = submitR [cr| let x = 1 + x                                    in showRule x |]
loop6  = submitR [cr| let x = array (0::Int, maxBound) [(1000000,'x')] in showRule x |]


-- forbidden codes
forbid1 = submitR "void $ runST (unsafeIOToST (readFile \"/etc/passwd\"))                     >>= outputAll_"
forbid2 = submitR "void $ unsafeCoerce (readFile \"/etc/passwd\")                             >>= outputAll_"
forbid3 = submitR "void $ Unsafe.unsafeCoerce (readFile \"/etc/passwd\")                      >>= outputAll_"
forbid4 = submitR "void $ Foreign.unsafePerformIO $ readFile \"/etc/passwd\"                  >>= outputAll_"
forbid5 = submitR "void $ Data.ByteString.Internal.inlinePerformIO (readFile \"/etc/passwd\") >>= outputAll_"
forbid6 = submitR "void $ unsafePerformIO (readFile \"/etc/passwd\")                          >>= outputAll_"

--an expression very long to type check
gameBadTypeCheck :: StateT Session IO ()
gameBadTypeCheck = submitR
   "void $ let {p x y f = f x y; f x = p x x} in f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f f)))))))))))))))))) f"

stackOverflow  = submitR [cr| let fix f = let x = f x in x                     in showRule $ foldr (.) id (repeat read) $ fix show |]
outputLimit  = submitR [cr| showRule $ repeat 1|]

--the game created should be withdrawn
condNoGame :: Multi -> Bool
condNoGame m = null $ _gameInfos m


-- ** File loading

--standard module
testFile1 :: StateT Session IO ()
testFile1 = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "myRule"

condNRules :: Int -> Multi -> Bool
condNRules n m = (length $ _rules $ firstGame m) == n

--standard module, call with namespace
testFile2 :: StateT Session IO ()
testFile2 = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "SimpleModule.myRule"


--loading two modules with the same name is forbidden
testFileTwice :: StateT Session IO ()
testFileTwice = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "SimpleModule.myRule"
   void $ testFile' "more/SimpleModule.hs" "SimpleModule.hs" "SimpleModule.myRule2"


--but having the same function name in different modules is OK
testFileTwice' :: StateT Session IO ()
testFileTwice' = do
   onePlayerOneGame
   void $ testFile "SimpleModule.hs" "SimpleModule.myRule"
   void $ testFile "SimpleModule2.hs" "SimpleModule2.myRule"

--security: no unsafe module imports
testFileUnsafeIO :: StateT Session IO ()
testFileUnsafeIO = do
   onePlayerOneGame
   void $ testFile "UnsafeIO.hs" "UnsafeIO.myRule"


-- * Helpers

--True if the string in parameter is among the outputs
isOutput' :: String -> Multi -> Bool
isOutput' s m = any (isOutput s . _game . _loggedGame) (_gameInfos m)

-- select first choice for all radio buttons
inputAllRadios :: Int -> PlayerNumber -> StateT Session IO ()
inputAllRadios choice pn = do
   s <- get
   let evs = evalState getChoiceEvents (EvalEnv 0 (firstGame $ _multi s))
   mapM_ (\(en, inum) -> inputResult pn en inum (RadioData choice) "test") evs

-- input text for all text fields
inputAllTexts :: String -> PlayerNumber -> StateT Session IO ()
inputAllTexts a pn = do
   s <- get
   let evs = evalState getTextEvents (firstGame $ _multi s)
   mapM_ (\(en, fa) -> inputResult pn en fa (TextData a) "test") evs

firstGame :: Multi -> Game
firstGame = G._game . _loggedGame . head . _gameInfos
