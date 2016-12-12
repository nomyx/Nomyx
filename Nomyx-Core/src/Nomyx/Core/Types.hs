{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances      #-}

module Nomyx.Core.Types where

import           Control.Lens hiding (Indexable)
import           Data.Acid                           (AcidState)
import           Data.Aeson.TH                       (defaultOptions, deriveJSON)
import           Data.Data                           (Data)
import           Data.IxSet                          (ixFun, ixSet, Indexable(..), IxSet)
import           Data.SafeCopy                       (base, deriveSafeCopy)
import           Data.Time
import           Data.List
import           Data.Typeable
import           GHC.Generics (Generic)
import           Language.Haskell.Interpreter.Server (ServerHandle)
import           Language.Nomyx
import           Network.BSD
import           Nomyx.Core.Engine
import           Data.Aeson

type PlayerPassword = String
type Port = Int
type CompileError = String
type LastRule = (RuleTemplate, String)

-- * Game structures

-- | Session contains all the game informations.
data Session = Session { _sh           :: ServerHandle,
                         _multi        :: Multi,
                         _acidProfiles :: AcidState ProfileDataState}

instance Show Session where
   show (Session _ m _) = show m

-- | A structure to hold the active games and players
data Multi = Multi { _gameInfos :: [GameInfo],
                     _mSettings :: Settings,
                     _mLibrary  :: Library}
                     deriving (Eq, Show, Typeable)

-- | Informations on a particular game
data GameInfo = GameInfo { _loggedGame     :: LoggedGame,
                           _ownedBy        :: Maybe PlayerNumber,
                           _forkedFromGame :: Maybe GameName,
                           _isPublic       :: Bool,
                           _startedAt      :: UTCTime}
                           deriving (Typeable, Show, Eq)

-- | Global settings
data Settings = Settings { _net           :: Network,  -- URL where the server is launched
                           _sendMails     :: Bool,     -- send mails or not
                           _adminPassword :: String,   -- admin password
                           _saveDir       :: FilePath, -- location of the save file, profiles and uploaded files
                           _webDir        :: FilePath, -- location of the website files
                           _sourceDir     :: FilePath, -- location of the language files, for display on the web gui (from Nomyx-Language)
                           _watchdog      :: Int}      -- time in seconds before killing the compilation thread
                           deriving (Eq, Show, Read, Typeable)

-- | Network infos
data Network = Network {_host :: HostName,
                        _port :: Port}
                        deriving (Eq, Show, Read, Typeable)

-- | The Library contains a list of rule templates together with their declarations
data Library = Library { _mTemplates :: [RuleTemplate],
                         _mModules   :: [ModuleInfo]}
                         deriving (Eq, Typeable)

instance Show Library where
   show (Library ts ms) = "\n\n Library Templates = " ++ (intercalate "\n " $ map show ts) ++
                          "\n\n Library Modules = "   ++ (intercalate "\n " $ map show ms)


-- * Player settings

data ProfileDataState = ProfileDataState { profilesData :: IxSet ProfileData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | 'ProfileData' contains player settings
data ProfileData =
    ProfileData { _pPlayerNumber   :: PlayerNumber, -- same as UserId
                  _pPlayerSettings :: PlayerSettings,
                  _pLastRule       :: Maybe LastRule,
                  _pLastUpload     :: LastUpload,
                  _pIsAdmin        :: Bool}
                  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Indexable ProfileData where
      empty =  ixSet [ ixFun (\(ProfileData pn _ _ _ _) -> [pn])]

-- Settings of a single player
data PlayerSettings =
   PlayerSettings { _pPlayerName    :: PlayerName,
                    _mail           :: Maybe String,
                    _mailNewInput   :: Bool,
                    _mailSubmitRule :: Bool,
                    _mailNewOutput  :: Bool,
                    _mailConfirmed  :: Bool}
                    deriving (Eq, Show, Read, Data, Ord, Typeable, Generic)

data LastUpload = NoUpload
                | UploadSuccess
                | UploadFailure (FilePath, CompileError)
                deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


$(deriveSafeCopy 1 'base ''LastUpload)
$(deriveSafeCopy 1 'base ''PlayerSettings)
$(deriveSafeCopy 1 'base ''ProfileData)
$(deriveSafeCopy 1 'base ''RuleTemplate)
$(deriveSafeCopy 1 'base ''ModuleInfo)
$(deriveSafeCopy 1 'base ''ProfileDataState)

makeLenses ''Multi
makeLenses ''Library
makeLenses ''ModuleInfo
makeLenses ''GameInfo
makeLenses ''Settings
makeLenses ''Network
makeLenses ''PlayerSettings
makeLenses ''Session
makeLenses ''ProfileData

$(deriveJSON defaultOptions ''Library)
$(deriveJSON defaultOptions ''GameInfo)
$(deriveJSON defaultOptions ''Multi)
$(deriveJSON defaultOptions ''Settings)
$(deriveJSON defaultOptions ''Network)
$(deriveJSON defaultOptions ''LastUpload)
$(deriveJSON defaultOptions ''PlayerSettings)
$(deriveJSON defaultOptions ''ProfileData)
$(deriveJSON defaultOptions ''RuleInfo)
$(deriveJSON defaultOptions ''RuleStatus)


instance ToJSON Rule where
   toJSON _ = object []

instance FromJSON Rule where
   parseJSON (Object _) = error "FromJSON"

