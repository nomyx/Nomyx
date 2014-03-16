{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where
import Language.Nomyx
import Language.Nomyx.Engine
import Data.Typeable
import Text.Reform.Happstack()
import Network.BSD
import Data.Lens.Template
import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Acid (AcidState)
import Happstack.Auth (ProfileState, AuthState)
import Data.Data (Data)
import Data.IxSet (inferIxSet, noCalcs)
import Data.SafeCopy (base, deriveSafeCopy)

type PlayerPassword = String
type Port = Int
type CompileError = String
type LastRule = (SubmitRule, String)


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
                           _sourceDir     :: FilePath} -- location of the language files, for display on the web gui (from Nomyx-Language)
                           deriving (Eq, Show, Read, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { _games   :: [LoggedGame],
                     _mSettings :: Settings}
                     deriving (Eq, Show, Typeable)

-- | 'ProfileData' contains application specific
data ProfileData =
    ProfileData { _pPlayerNumber   :: PlayerNumber, -- ^ same as UserId
                  _pPlayerSettings :: PlayerSettings,
                  _pViewingGame    :: Maybe GameName,
                  _pLastRule       :: Maybe LastRule,
                  _pLastUpload     :: LastUpload,
                  _pIsAdmin        :: Bool
                  }
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

instance Show Session where
   show (Session _ m _) = show m

$( makeLenses [''Multi, ''Settings, ''Network, ''PlayerSettings, ''Session, ''ProfileData] )


