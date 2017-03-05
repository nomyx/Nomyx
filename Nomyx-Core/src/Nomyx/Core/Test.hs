{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Test module
module Nomyx.Core.Test where

import           Control.Monad.State
import           Control.Exception as E
import           Control.Lens
import           Language.Haskell.TH hiding (ModuleInfo)
import           Language.Haskell.TH.Syntax as THS hiding (lift, Module, ModuleInfo)
import           System.IO.Unsafe
import           Data.List
import           Data.Maybe
import qualified Data.Text.IO as DT
import           Data.Acid
import           Data.Acid.Memory
import           Data.Time hiding (getCurrentTime)
import           Paths_Nomyx_Core as PNC
import           System.IO.Temp
import           System.FilePath ((</>))
import           System.Directory (createDirectoryIfMissing)
import           Nomyx.Language hiding (getCurrentTime)
import           Nomyx.Core.Types
import           Nomyx.Core.Multi
import           Nomyx.Core.Session
import           Nomyx.Core.Utils
import           Nomyx.Core.Profile
import           Nomyx.Core.Quotes
import           Nomyx.Core.Engine
import qualified Nomyx.Core.Engine as G
import           Imprevu.Evaluation


playTests :: Maybe String -> Int -> IO [(String, Bool)]
playTests mTestName delay = do
   tests <- case mTestName of
      Just testName -> do
         let tsts = fatalTests ++ regularTests
         return $ maybeToList $ find (\(name, _, _) -> name == testName) tsts
      Nothing -> return regularTests
   tp <- testProfiles
   let session = Session (defaultMulti Settings {_net = defaultNetwork, _mailSettings = (MailSettings False "" "" ""), _adminPassword = "", _saveDir = "", _webDir = "", _sourceDir = "", _watchdog = delay}
                                          (Library [rAutoActivate]
                                          [])) tp
   mapM (\(title, t, cond) -> (title,) <$> test title session t cond) tests

defaultNetwork :: Network
defaultNetwork = Network "" 0

-- | test list.
-- each test can be loaded individually in Nomyx with the command line:
-- Nomyx -l <"test name">
regularTests :: [(String, StateT Session IO (), Multi -> Bool)]
regularTests =
   [("hello World",           gameHelloWorld,         condHelloWorld),
    ("hello World 2 players", gameHelloWorld2Players, condHelloWorld2Players),
    ("Partial Function 1",    gamePartialFunction1,   condPartialFunction),
    ("Partial Function 2",    gamePartialFunction2,   condPartialFunction),
    ("Partial Function 3",    gamePartialFunction3,   condPartialFunction3),
    ("Test file 1",           testFile1,              condNRules 2),
    ("Test file 2",           testFile2,              condNRules 2),
    ("Test import Data.Time", testFileTime,           condNRules 2),
    ("load file twice 2",     testFileTwice',         condNRules 3),
    ("load file unsafe",      testFileUnsafeIO,       condNRules 1)] ++
    map (\i -> ("Loop" ++ show i,      loops !! (i-1),      condNoGame))   [1..(length loops)] ++
    map (\i -> ("Forbidden" ++ show i, forbiddens !! (i-1), condNRules 1)) [1..(length forbiddens)]

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
   let delay = _watchdog $ _mSettings $ _multi s
   ms <- evalWithWatchdog delay s (evalSession tes) --version with no watchdog: ms <- Just <$> execStateT tes s
   case ms of
      Just s' -> return $ _multi s'
      Nothing -> do
         putStrLn "thread timed out, session discarded"
         return $ _multi s

testException :: Multi -> SomeException -> IO Multi
testException m e = do
   putStrLn $ "Test Exception: " ++ show e
   return m

testProfiles :: IO (AcidState ProfileDataState)
testProfiles = openMemoryState initialProfileDataState

printRule :: Q THS.Exp -> String
printRule r = unsafePerformIO $ do
   expr <- runQ r
   return $ pprint expr

onePlayerOneGame :: StateT Session IO ()
onePlayerOneGame = do
   newPlayer 1 PlayerSettings {_pPlayerName = "Player 1", _mail = Nothing, _mailNewInput = False, _mailSubmitRule = False, _mailNewOutput = False, _mailConfirmed = False}
   newGame "test" (GameDesc "" "") 1 True
   joinGame "test" 1

twoPlayersOneGame :: StateT Session IO ()
twoPlayersOneGame = do
   onePlayerOneGame
   newPlayer 2 PlayerSettings {_pPlayerName = "Player 2", _mail = Nothing, _mailNewInput = False, _mailSubmitRule = False, _mailNewOutput = False, _mailConfirmed = False}
   joinGame "test" 2

submitR :: String -> StateT Session IO ()
submitR r = do
   onePlayerOneGame
   submitRule (RuleTemplate "" "" r "" Nothing [] []) 1 "test"

testFile' :: FilePath -> FilePath -> String -> StateT Session IO ()
testFile' path name func = do
   dataDir <- lift PNC.getDataDir
   cont <- liftIO $ DT.readFile (dataDir </> testDir </> path)
   updateLibrary 1 (Library [] [ModuleInfo name cont])
   submitRule (RuleTemplate "" "" func "" Nothing [] [name]) 1 "test"

testFile :: FilePath -> String -> StateT Session IO ()
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
   submitRule (RuleTemplate "" "" [cr|helloWorld|] "" Nothing [] []) 1 "test"

condHelloWorld2Players :: Multi -> Bool
condHelloWorld2Players = isOutput' "hello, world!"

condMoneyTransfer :: Multi -> Bool
condMoneyTransfer m = (_vName $ head $ _variables $ firstGame m) == "Accounts"

-- ** Partial functions

partialFunction1 :: String
partialFunction1 = [cr|void $ readVar_ (V "toto1" :: V String)|]

partialFunction2 :: String
partialFunction2 = [cr|void $ do
   t <- getCurrentTime
   onEventOnce (timeEvent $ addUTCTime 5 t) $ const $ readVar_ (V "toto2")|]

gamePartialFunction1 :: StateT Session IO ()
gamePartialFunction1 = submitR partialFunction1

gamePartialFunction2 :: StateT Session IO ()
gamePartialFunction2 = do
   onePlayerOneGame
   submitR partialFunction2
   gs <- (use $ multi . gameInfos)
   let now = _currentTime $ G._game $ _loggedGame $ head gs
   return ()
--   zoom multi $ Nomyx.Core.Multi.triggerTimeEvent (5 `addUTCTime` now)
--   triggerTimeEvent defaultEvalConf) (EvalState g 0) ts

-- rule has not been accepted due to exception
condPartialFunction :: Multi -> Bool
condPartialFunction m = (_rStatus $ head $ _rules $ firstGame m) == Active &&
                        (take 5 $ _lMsg $ head $ _logs $ firstGame m) == "Error"


partialFunction3 :: String
partialFunction3 = [cr|void $ onEvent_ (ruleEvent Proposed) $ const $ readVar_ (V "toto3")|]

gamePartialFunction3 :: StateT Session IO ()
gamePartialFunction3 = do
   submitR partialFunction3
   submitR [cr|nothing|]

-- rule has been accepted and also next one
condPartialFunction3 :: Multi -> Bool
condPartialFunction3 m = (length $ _rules $ firstGame m) == 3

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
   testFile "SimpleModule.hs" "myRule"

condNRules :: Int -> Multi -> Bool
condNRules n m = (length $ _rules $ firstGame m) == n

--standard module, call with namespace
testFile2 :: StateT Session IO ()
testFile2 = do
   onePlayerOneGame
   testFile "SimpleModule.hs" "SimpleModule.myRule"


--module that imports Data.Time (should be Safe in recent versions)
testFileTime :: StateT Session IO ()
testFileTime = do
   onePlayerOneGame
   testFile "TestTime.hs" "TestTime.myRule"

--loading two modules with the same name is forbidden
testFileTwice :: StateT Session IO ()
testFileTwice = do
   onePlayerOneGame
   testFile "SimpleModule.hs" "SimpleModule.myRule"
   testFile' "more/SimpleModule.hs" "SimpleModule.hs" "SimpleModule.myRule2"


--but having the same function name in different modules is OK
testFileTwice' :: StateT Session IO ()
testFileTwice' = do
   onePlayerOneGame
   testFile "SimpleModule.hs" "SimpleModule.myRule"
   testFile "SimpleModule2.hs" "SimpleModule2.myRule"

--security: no unsafe module imports
testFileUnsafeIO :: StateT Session IO ()
testFileUnsafeIO = do
   onePlayerOneGame
   testFile "UnsafeIO.hs" "UnsafeIO.myRule"


-- * Helpers

--True if the string in parameter is among the outputs
isOutput' :: String -> Multi -> Bool
isOutput' s m = any (isOutput s . _game . _loggedGame) (_gameInfos m)

firstGame :: Multi -> Game
firstGame = G._game . _loggedGame . head . _gameInfos
