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

{-# LANGUAGE NamedFieldPuns #-}
    
module Utils where

import Data.Maybe
import Control.Monad.State
import Types
import Language.Nomyx
import Language.Nomyx.Game
import Data.Lens
import Control.Category hiding ((.))
import Safe
import Control.Concurrent.STM
import qualified Data.Acid.Advanced as A (query', update')
import System.IO.Temp
import Codec.Archive.Tar as Tar
import System.Directory
import System.Posix
import System.FilePath

saveFile, profilesDir, uploadDir, tarFile :: FilePath
saveFile    = "Nomyx.save"
profilesDir = "profiles"
uploadDir   = "uploads"
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

getPlayerName :: PlayerNumber -> Session -> IO PlayerName
getPlayerName pn s = do
   pfd <- A.query' (acidProfileData $ _profiles s) (AskProfileData pn)
   return $ _pPlayerName $ _pPlayerSettings $ fromJustNote ("getPlayersName: no profile for pn=" ++ (show pn)) pfd

getPlayerInGameName :: Game -> PlayerNumber -> PlayerName
getPlayerInGameName g pn = do
   case find ((==pn) . getL playerNumber) (_players g) of
      Nothing -> error "getPlayersName': No player by that number in that game"
      Just pm -> _playerName pm

-- | returns the game the player is in
getPlayersGame :: PlayerNumber -> Session -> IO (Maybe LoggedGame)
getPlayersGame pn s = do
   pfd <- A.query' (acidProfileData $ _profiles s) (AskProfileData pn)
   let mgn = _pViewingGame $ fromJustNote "getPlayersGame" pfd
   return $ do
      gn <- mgn
      find ((== gn) . getL (game >>> gameName)) (_games $ _multi s) --checks if any game by that name exists

getAllProfiles :: Session -> IO [ProfileData]
getAllProfiles s = A.query' (acidProfileData $ _profiles s) AskProfilesData


getPlayer :: Game -> PlayerNumber -> Maybe PlayerInfo
getPlayer g pn = find ((==pn) . getL playerNumber) (_players g)

-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: LoggedGame -> StateT Multi IO ()
modifyGame lg = do
   gs <- access games
   case find (== lg) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg lg gs
         games ~= newgs
         return ()

execWithMulti :: UTCTime -> StateT Multi IO () -> Multi -> IO Multi
execWithMulti t ms m = do
   let setTime g = (game >>> currentTime) ^= t $ g
   let m' = games `modL` (map setTime) $ m
   execStateT ms m'

modifyProfile :: PlayerNumber -> (ProfileData -> ProfileData) -> StateT Session IO ()
modifyProfile pn mod = do
   s <- get
   pfd <- A.query' (acidProfileData $ _profiles s) (AskProfileData pn)
   when (isJust pfd) $ void $ A.update' (acidProfileData $ _profiles s) (SetProfileData (mod $ fromJust pfd))

getProfile :: MonadIO m => Session -> PlayerNumber -> m (Maybe ProfileData)
getProfile s pn = A.query' (acidProfileData $ _profiles s) (AskProfileData pn)

getProfile' :: MonadIO m => (TVar Session) -> PlayerNumber -> m (Maybe ProfileData)
getProfile' ts pn = do
   s <- liftIO $ atomically $ readTVar ts
   getProfile s pn

getSaveFile :: Settings -> FilePath
getSaveFile set = (_saveDir set) </> saveFile

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
   filterM (getFileStatus . (\f -> joinPath [saveDir, uploadDir, f]) >=> return . isRegularFile) $ mods

