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
import Control.Monad.CatchIO
import System.PosixCompat.Files (getFileStatus, isRegularFile, setFileMode, ownerModes, groupModes)
#ifndef WINDOWS
import qualified System.Posix.Signals as S
#endif

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
protectHandlers a = bracket saveHandlers restoreHandlers $ const a

#endif
