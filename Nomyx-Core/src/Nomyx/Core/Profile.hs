{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Nomyx.Core.Profile where

import Language.Nomyx
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Catch (bracket)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
import Control.Lens
import Safe
import Data.List
import Data.Maybe
import Data.IxSet (toList, (@=))
import qualified Data.IxSet  as IxSet
import qualified Data.Acid (AcidState(..))
import qualified Data.Acid.Advanced as A (update', query')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid (openLocalStateFrom, makeAcidic, Update, Query)
import Happstack.Authenticate.Core (initialAuthenticateState, AuthenticateState)
--import Happstack.Auth.Core.Profile (initialProfileState)
import System.FilePath ((</>))
import Nomyx.Core.Quotes
import Nomyx.Core.Types
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

initialProfileData :: PlayerNumber -> PlayerSettings -> ProfileData
initialProfileData uid ps = ProfileData uid ps (Just (exampleRule, "")) NoUpload False

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
defaultPlayerSettings = PlayerSettings "" Nothing False False False False


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

getAllProfiles :: Session -> IO [ProfileData]
getAllProfiles s = A.query' (acidProfileData $ _profiles s) AskProfilesData

getPlayerInfo :: Game -> PlayerNumber -> Maybe PlayerInfo
getPlayerInfo g pn = find ((==pn) . getL playerNumber) (_players g)

withAcid :: Maybe FilePath -- ^ state directory
         -> Data.Acid.AcidState AuthenticateState
         -> (Profiles -> IO a) -- ^ action
         -> IO a
withAcid mBasePath authenticateState f = do
    let basePath = fromMaybe "_state" mBasePath
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState)  createCheckpointAndClose $ \profileData ->
        f (Profiles authenticateState profileData)
