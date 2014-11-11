{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Nomyx.Core.Types where

import Network.BSD
import Language.Haskell.Interpreter.Server (ServerHandle)
import Happstack.Auth (ProfileState, AuthState)
import Data.Typeable
import Data.Lens.Template
import Data.Acid (AcidState)
import Data.Data (Data)
import Data.IxSet (inferIxSet, noCalcs)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time
import Language.Nomyx
import Nomyx.Core.Engine

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
                           _saveDir       :: FilePath, -- location of the save file, profiles and uploaded files
                           _webDir        :: FilePath, -- location of the website files
                           _sourceDir     :: FilePath} -- location of the language files, for display on the web gui (from Nomyx-Language)
                           deriving (Eq, Show, Read, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { _gameInfos :: [GameInfo],
                     _mSettings  :: Settings}
                     deriving (Eq, Show, Typeable)

data GameInfo = GameInfo { _loggedGame     :: LoggedGame,
                           _ownedBy        :: Maybe PlayerNumber,
                           _forkedFromGame :: Maybe GameName,
                           _isPublic       :: Bool,
                           _startedAt      :: UTCTime}
                           deriving (Typeable, Show, Eq)


-- | 'ProfileData' contains application specific
data ProfileData =
    ProfileData { _pPlayerNumber   :: PlayerNumber, -- ^ same as UserId
                  _pPlayerSettings :: PlayerSettings,
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

$( makeLenses [''Multi, ''GameInfo, ''Settings, ''Network, ''PlayerSettings, ''Session, ''ProfileData] )

