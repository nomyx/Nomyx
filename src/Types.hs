{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, TypeFamilies,
             NamedFieldPuns, TemplateHaskell, FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving #-}

module Types where
import Language.Nomyx
import Data.Typeable
import Text.Reform.Happstack()
import Network.BSD
import Data.Lens.Template
import Language.Nomyx.Engine
import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Acid (makeAcidic, Update, Query, AcidState)
import Happstack.Auth (ProfileState, AuthState)
import Data.Data (Data)
import Data.IxSet (toList, (@=), inferIxSet, noCalcs)
import qualified Data.IxSet  as IxSet
import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Safe

type PlayerPassword = String
type Port = Int
type CompileError = String
type LastRule = (SubmitRule, CompileError)


data LastUpload = NoUpload
                | UploadSuccess
                | UploadFailure (FilePath, CompileError)
                deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''LastUpload)

data Network = Network {_host :: HostName, _port :: Port}
               deriving (Eq, Show, Read, Typeable)
defaultNetwork = Network "" 0

data PlayerSettings =
   PlayerSettings { _pPlayerName   :: PlayerName,
                    _mail          :: String,
                    _mailNewInput  :: Bool,
                    _mailNewRule   :: Bool,
                    _mailNewOutput :: Bool,
                    _mailConfirmed :: Bool}
                    deriving (Eq, Show, Read, Data, Ord, Typeable)
$(deriveSafeCopy 1 'base ''PlayerSettings)


data Settings = Settings { _net           :: Network,  -- URL where the server is launched
                           _sendMails     :: Bool,     -- send mails or not
                           _adminPassword :: String,   -- admin password
                           _saveDir       :: FilePath, -- location of the save file and uploaded files
                           _dataDir       :: FilePath, -- location of the static files (profiles + website)
                           _sourceDir     :: FilePath} -- location of the language files (from Nomyx-Language)
                           deriving (Eq, Show, Read, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { _games   :: [LoggedGame],
                     _mSettings :: Settings}
                     deriving (Eq, Read, Show, Typeable)

data Admin = Admin { _isAdmin :: Bool,
                     _pPlayAs :: Maybe PlayerNumber}
                     deriving (Eq, Show, Read, Ord, Typeable, Data)
$(deriveSafeCopy 1 'base ''Admin)

defaultAdmin :: Admin
defaultAdmin = Admin False Nothing

-- | 'ProfileData' contains application specific
data ProfileData =
    ProfileData { _pPlayerNumber   :: PlayerNumber, -- ^ same as UserId
                  _pPlayerSettings :: PlayerSettings,
                  _pViewingGame    :: Maybe GameName,
                  _pLastRule       :: Maybe LastRule,
                  _pLastUpload     :: LastUpload,
                  _pAdmin          :: Admin}
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileData)
$(deriveSafeCopy 1 'base ''SubmitRule)

$(inferIxSet "ProfilesData" ''ProfileData 'noCalcs [''PlayerNumber]) -- , ''Text

data ProfileDataState =
    ProfileDataState { profilesData :: ProfilesData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileDataState)


-- | 'Acid' holds all the 'AcidState' handles for this site.
data Profiles = Profiles
    { acidAuth        :: AcidState AuthState,
      acidProfile     :: AcidState ProfileState,
      acidProfileData :: AcidState ProfileDataState}

data Session = Session { _sh :: ServerHandle,
                         _multi :: Multi,
                         _profiles  :: Profiles}

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

-- | create the profile data, but only if it is missing
newProfileData :: PlayerNumber -> PlayerSettings -> Update ProfileDataState ProfileData
newProfileData uid ps =
    do pds@(ProfileDataState {..}) <- get
       case IxSet.getOne (profilesData @= uid) of
         Nothing -> do let profileData = ProfileData uid ps Nothing Nothing NoUpload defaultAdmin
                       put $ pds { profilesData = IxSet.updateIx uid profileData profilesData }
                       return profileData
         (Just profileData) -> return profileData

-- | get number of
askProfileDataNumber :: Query ProfileDataState Int
askProfileDataNumber =
    do pds <- ask
       tracePN 1 (show $ profilesData pds)
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


defaultMulti :: Settings -> Multi
defaultMulti set = Multi [] set

defaultPlayerSettings :: PlayerSettings
defaultPlayerSettings = PlayerSettings "" "" False False False False

$( makeLenses [''Multi, ''Settings, ''Network, ''PlayerSettings, ''Session, ''ProfileData, ''Admin] )


