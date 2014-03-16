-----------------------------------------------------------------------------
--
-- Module      :  Utils
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -cpp #-}

module Utils where

import Data.Maybe
import Control.Monad.State
import Types
import System.IO.Temp
import Codec.Archive.Tar as Tar
import System.Directory
import System.FilePath
import Control.Monad.CatchIO as MC
#ifndef WINDOWS
import qualified System.Posix.Signals as S
#endif
import Control.Concurrent
import System.IO
import System.IO.PlafCompat
import Control.Exception

saveFile, profilesDir, uploadDir, tarFile :: FilePath
saveFile    = "Nomyx.save"
profilesDir = "profiles"
uploadDir   = "uploads"
testDir     = "test"
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
nomyxURL (Network host port) = "http://" ++ host ++ ":" ++ (show port)

getSaveFile :: Settings -> FilePath
getSaveFile set = (_saveDir set) </> saveFile

getTestDir :: Settings -> FilePath
getTestDir set = (_dataDir set) </> testDir

makeTar :: FilePath -> IO ()
makeTar saveDir = do
   putStrLn $ "creating tar in " ++ (show saveDir)
   Tar.create (saveDir </> tarFile) saveDir [saveFile, uploadDir]

untar :: FilePath -> IO (FilePath)
untar fp = do
   dir <- createTempDirectory "/tmp" "Nomyx"
   extract dir fp
   return dir

getUploadedModules :: FilePath -> IO [FilePath]
getUploadedModules saveDir = do
   mods <- getDirectoryContents $ saveDir </> uploadDir
   getRegularFiles (saveDir </> uploadDir) mods

getRegularFiles :: FilePath -> [FilePath] -> IO [FilePath]
getRegularFiles dir fps = filterM (getFileStatus . (\f -> dir </> f) >=> return . isRegularFile) $ fps

setMode :: FilePath -> IO()
setMode file = setFileMode file (ownerModes + groupModes)

#ifdef WINDOWS

--no signals under windows
protectHandlers :: MonadCatchIO m => m a -> m a
protectHandlers = id

#else

installHandler' :: MonadCatchIO m => S.Handler -> S.Signal -> m S.Handler
installHandler' handler signal = liftIO $ S.installHandler signal handler Nothing

signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]

saveHandlers :: MonadCatchIO m => m [S.Handler]
saveHandlers = liftIO $ mapM (installHandler' S.Ignore) signals

restoreHandlers :: MonadCatchIO m => [S.Handler] -> m [S.Handler]
restoreHandlers h  = liftIO . sequence $ zipWith installHandler' h signals


protectHandlers :: MonadCatchIO m => m a -> m a
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
   putStrLn "Starting watchdog eval"
   mvar <- newEmptyMVar
   hSetBuffering stdout NoBuffering
   --start evaluation thread
   id <- forkOS $ do
      --setResourceLimit ResourceCPUTime (ResourceLimits (ResourceLimit 3) (ResourceLimit 4))
      s' <- f s
      s'' <- evaluate s'
      writeFile nullFileName $ show s''
      putMVar mvar (Just s'')
   --start watchdog thread
   forkIO $ watchDog 3 id mvar
   takeMVar mvar

-- | Fork off a thread which will sleep and then kill off the specified thread.
watchDog :: Int -> ThreadId -> MVar (Maybe a) -> IO ()
watchDog tout tid mvar = do
   threadDelay (tout * 1000000)
   killThread tid
   putMVar mvar Nothing
