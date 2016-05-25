
{-# LANGUAGE CPP #-}

module Nomyx.Core.Utils where



import           Codec.Archive.Tar    as Tar
import           System.Directory
import           System.FilePath
import           System.IO.Temp
#ifndef WINDOWS
import qualified System.Posix.Signals as S
#endif
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad.Catch  as MC
import           Control.Monad.State
import           Data.Maybe
import           Nomyx.Core.Engine
import           Nomyx.Core.Types
import           System.IO
import           System.IO.PlafCompat
import           Data.List

saveFile, profilesDir, uploadDir, testDir, authDir, tarFile :: FilePath
saveFile    = "Nomyx.save"
profilesDir = "profiles"
uploadDir   = "uploads"
testDir     = "test"
authDir     = "authenticate"
tarFile     = "Nomyx.tar"

-- | this function will return just a if it can cast it to an a.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a => a   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

-- | generic function to say things on transformers like GameState, ServerState etc.
say :: String -> StateT a IO ()
say = lift . putStrLn

nomyxURL :: Network -> String
nomyxURL (Network h p) = "http://" ++ h ++ ":" ++ show p

getSaveFile :: Settings -> FilePath
getSaveFile s = _saveDir s </> saveFile

makeTar :: FilePath -> IO ()
makeTar saveDir = do
   putStrLn $ "creating tar in " ++ show saveDir
   Tar.create (saveDir </> tarFile) saveDir [saveFile, uploadDir]

untar :: FilePath -> IO FilePath
untar fp = do
   dir <- createTempDirectory "/tmp" "Nomyx"
   extract dir fp
   return dir

getUploadedModules :: FilePath -> IO [FilePath]
getUploadedModules saveDir = do
   mods <- getDirectoryContents $ saveDir
   getRegularFiles (saveDir) mods

getRegularFiles :: FilePath -> [FilePath] -> IO [FilePath]
getRegularFiles dir fps = filterM (getFileStatus . (\f -> dir </> f) >=> return . isRegularFile) fps

-- The setMode function is only used during module loading. In Windows,
-- a copied file automatically inherits permissions based on the containing
-- folder's ACLs an the user account being used.
#ifdef WINDOWS

setMode :: FilePath -> IO()
setMode _ = return ()

#else

setMode :: FilePath -> IO()
setMode file = setFileMode file (ownerModes + groupModes)

#endif

#ifdef WINDOWS

--no signals under windows
protectHandlers :: IO a -> IO a
protectHandlers = id

#else

installHandler' :: S.Handler -> S.Signal -> IO S.Handler
installHandler' handler signal = S.installHandler signal handler Nothing

signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]

saveHandlers :: IO [S.Handler]
saveHandlers = liftIO $ mapM (installHandler' S.Ignore) signals

restoreHandlers :: [S.Handler] -> IO [S.Handler]
restoreHandlers h  = liftIO . sequence $ zipWith installHandler' h signals

protectHandlers :: IO a -> IO a
protectHandlers a = MC.bracket saveHandlers restoreHandlers $ const a

#endif

--Sets a watchdog to kill the evaluation thread if it doesn't finishes.
-- The function starts both the evaluation thread and the watchdog thread, and blocks awaiting the result.
-- Option 1: the evaluation thread finishes before the watchdog. The MVar is filled with the result,
--  which unblocks the main thread. The watchdog then finishes latter, and fills the MVar with Nothing.
-- Option 2: the watchdog finishes before the evaluation thread. The eval thread is killed, and the
--  MVar is filled with Nothing, which unblocks the main thread. The watchdog finishes.
evalWithWatchdog :: Show b => a -> (a -> IO b) -> IO (Maybe b)
evalWithWatchdog s f = do
   mvar <- newEmptyMVar
   hSetBuffering stdout NoBuffering
   --start evaluation thread
   id <- forkIO $ do
      s' <- f s
      s'' <- evaluate s'
      writeFile nullFileName $ show s''
      putMVar mvar (Just s'')
   --start watchdog thread
   forkIO $ watchDog 5 id mvar
   takeMVar mvar

evalWithWatchdog' :: NFData a => IO a -> IO (Maybe a)
evalWithWatchdog' s = do
   mvar <- newEmptyMVar
   hSetBuffering stdout NoBuffering
   --start evaluation thread
   id <- forkIO $ do
      s' <- s
      let s'' = force s'
      putMVar mvar (Just s'')
   --start watchdog thread
   forkIO $ watchDog 5 id mvar
   takeMVar mvar


-- | Fork off a thread which will sleep n seconds and then kill off the specified thread.
watchDog :: Int -> ThreadId -> MVar (Maybe a) -> IO ()
watchDog n tid mvar = do
   threadDelay (n * 1000000)
   killThread tid
   threadDelay 1000000 --give some time to kill the thread
   putMVar mvar Nothing

gameNameLens :: Lens' GameInfo GameName
gameNameLens = loggedGame . game . gameName

getGameByName :: GameName -> Session -> Maybe GameInfo
getGameByName gn s = find ((==gn) . getL (loggedGame . game . gameName)) (_gameInfos $ _multi s)
