{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Nomyx.Core.Profile where

import Language.Nomyx
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Catch (bracket)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
import Safe
import Data.Lens
import Data.List
import Data.Maybe
import Data.IxSet (toList, (@=))
import qualified Data.IxSet  as IxSet
import qualified Data.Acid.Advanced as A (update', query')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid (openLocalStateFrom, makeAcidic, Update, Query)
import Happstack.Auth.Core.Auth (initialAuthState)
import Happstack.Auth.Core.Profile (initialProfileState)
import System.FilePath ((</>))
import Nomyx.Core.Quotes
import Nomyx.Core.Types
import Nomyx.Core.Utils
import Nomyx.Core.Engine

-- | set 'ProfileData' for UserId
setProfileData :: ProfileData -> Update ProfileDataState ProfileData
setProfileData profileData =
    do pds@(ProfileDataState {..}) <- get
       put $ pds { profilesData = IxSet.updateIx (_pPlayerNumber profileData) profileData profilesData }
       return profileData

-- | get 'ProfileData' associated with 'UserId'
askProfileData :: PlayerNumber -> Query ProfileDataState (Maybe ProfileData)
askProfileData uid = do
   ProfileDataState{..} <- ask
   let pfs = toList profilesData
   let filtered = filter (\a -> _pPlayerNumber a == uid) pfs
   return $ headMay filtered
   --return $ getOne $ profilesData @= uid

initialProfileData :: PlayerNumber -> PlayerSettings -> ProfileData
initialProfileData uid ps = ProfileData uid ps Nothing (Just (exampleRule, "")) NoUpload False

exampleRule :: SubmitRule
exampleRule = SubmitRule "" "" [cr|
--This is an example new rule that you can enter.
--If you submit this rule it will have to be voted on by other players (as described by rule 1).
--A lot of other examples can be found in the left menu bar.
do
   --get your own player number
   me <- getProposerNumber_
   --create an output for me only
   let displayMsg _ = void $ newOutput_ (Just me) "Bravo!"
   --create a button for me, which will display the output when clicked
   void $ onInputButton_ "Click here:" displayMsg me
|]


-- | create the profile data, but only if it is missing
newProfileData :: PlayerNumber -> PlayerSettings -> Update ProfileDataState ProfileData
newProfileData uid ps =
    do pds@(ProfileDataState {..}) <- get
       case IxSet.getOne (profilesData @= uid) of
         Nothing -> do
            let pd = initialProfileData uid ps
            put $ pds { profilesData = IxSet.updateIx uid pd profilesData }
            return pd
         Just profileData -> return profileData

-- | get number of
askProfileDataNumber :: Query ProfileDataState Int
askProfileDataNumber =
    do pds <- ask
       return $ IxSet.size $ profilesData pds

-- | get all profiles
askProfilesData :: Query ProfileDataState [ProfileData]
askProfilesData =
    do pds <- ask
       return $ toList $ profilesData pds

$(makeAcidic ''ProfileDataState
                [ 'setProfileData
                , 'askProfileData
                , 'newProfileData
                , 'askProfileDataNumber
                , 'askProfilesData
                ]
 )

initialProfileDataState :: ProfileDataState
initialProfileDataState = ProfileDataState { profilesData = IxSet.empty }

defaultPlayerSettings :: PlayerSettings
defaultPlayerSettings = PlayerSettings "" "" False False False False


modifyProfile :: PlayerNumber -> (ProfileData -> ProfileData) -> StateT Session IO ()
modifyProfile pn mod = do
   s <- get
   pfd <- A.query' (acidProfileData $ _profiles s) (AskProfileData pn)
   when (isJust pfd) $ void $ A.update' (acidProfileData $ _profiles s) (SetProfileData (mod $ fromJust pfd))

getProfile :: MonadIO m => Session -> PlayerNumber -> m (Maybe ProfileData)
getProfile s pn = A.query' (acidProfileData $ _profiles s) (AskProfileData pn)

getProfile' :: MonadIO m => TVar Session -> PlayerNumber -> m (Maybe ProfileData)
getProfile' ts pn = do
   s <- liftIO $ atomically $ readTVar ts
   getProfile s pn

getPlayerName :: PlayerNumber -> Session -> IO PlayerName
getPlayerName pn s = do
   pfd <- A.query' (acidProfileData $ _profiles s) (AskProfileData pn)
   return $ _pPlayerName $ _pPlayerSettings $ fromJustNote ("getPlayersName: no profile for pn=" ++ show pn) pfd

getPlayerInGameName :: Game -> PlayerNumber -> PlayerName
getPlayerInGameName g pn = case find ((==pn) . getL playerNumber) (_players g) of
   Nothing -> error "getPlayersName': No player by that number in that game"
   Just pm -> _playerName pm

-- | returns the game the player is in
getPlayersGame :: PlayerNumber -> Session -> IO (Maybe GameInfo)
getPlayersGame pn s = do
   pfd <- A.query' (acidProfileData $ _profiles s) (AskProfileData pn)
   let mgn = _pViewingGame $ fromJustNote "getPlayersGame" pfd
   return $ do
      gn <- mgn
      find ((== gn) . getL gameNameLens) (_gameInfos $ _multi s) --checks if any game by that name exists

getAllProfiles :: Session -> IO [ProfileData]
getAllProfiles s = A.query' (acidProfileData $ _profiles s) AskProfilesData

getPlayerInfo :: Game -> PlayerNumber -> Maybe PlayerInfo
getPlayerInfo g pn = find ((==pn) . getL playerNumber) (_players g)

withAcid :: Maybe FilePath -- ^ state directory
         -> (Profiles -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        createCheckpointAndClose $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     createCheckpointAndClose $ \profile ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) createCheckpointAndClose $ \profileData ->
        f (Profiles auth profile profileData)


