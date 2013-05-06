
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, TypeFamilies,
             NamedFieldPuns, TemplateHaskell, FlexibleContexts, RecordWildCards #-}

module Types where
import Language.Nomyx
import Data.Typeable
import Text.Blaze.Html5 hiding (map, label, base)
import Text.Reform
import Happstack.Server
import Text.Reform.Happstack()
import Network.BSD
import Data.Lens.Template
import Language.Nomyx.Game
import Language.Haskell.Interpreter.Server (ServerHandle)
import Data.Acid (makeAcidic, Update, Query, AcidState)
import Happstack.Auth (ProfileState, AuthState)
import Data.Text (Text)
import Data.Data (Data)
import Data.IxSet (toList, (@=), getOne, inferIxSet, noCalcs)
import qualified Data.IxSet  as IxSet
import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))

type PlayerPassword = String
type Port = Int
data Network = Network {_host :: HostName, _port :: Port}
               deriving (Eq, Show, Read, Typeable)
defaultNetwork = Network "" 0

data PlayerSettings =
   PlayerSettings { _pPlayerName   :: PlayerName,
                    _mailTo :: String,
                    _mailNewInput :: Bool,
                    _mailNewRule :: Bool,
                    _mailNewOutput :: Bool,
                    _mailConfirmed :: Bool }
                    deriving (Eq, Show, Read, Data, Ord, Typeable)
$(deriveSafeCopy 1 'base ''PlayerSettings)


data Settings = Settings { _logFilePath :: FilePath,
                           _net :: Network,
                           _sendMails :: Bool}
                           deriving (Eq, Show, Read, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { _games   :: [LoggedGame],
                     _mSettings :: Settings,
                     _mCurrentTime :: UTCTime}
                     deriving (Eq, Read, Show, Typeable)


-- | 'ProfileData' contains application specific
data ProfileData =
    ProfileData { _pPlayerNumber   :: PlayerNumber, -- ^ same as UserId
                  _pPlayerSettings :: PlayerSettings,
                  _pViewingGame    :: Maybe GameName,
                  _pLastRule       :: Maybe SubmitRule}
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileData)
$(deriveSafeCopy 1 'base ''SubmitRule)

$(inferIxSet "ProfilesData" ''ProfileData 'noCalcs [''PlayerNumber, ''Text])

data ProfileDataState =
    ProfileDataState { profilesData :: ProfilesData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileDataState)


-- | 'Acid' holds all the 'AcidState' handles for this site.
data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    , acidProfileData :: AcidState ProfileDataState
    }


-- | set 'ProfileData' for UserId
setProfileData :: ProfileData -> Update ProfileDataState ProfileData
setProfileData profileData =
    do pds@(ProfileDataState {..}) <- get
       put $ pds { profilesData = IxSet.updateIx (_pPlayerNumber profileData) profileData profilesData }
       return profileData


-- | get 'ProfileData' associated with 'UserId'
askProfileData :: PlayerNumber -> Query ProfileDataState (Maybe ProfileData)
askProfileData uid =
    do ProfileDataState{..} <- ask
       return $ getOne $ profilesData @= uid

-- | create the profile data, but only if it is missing
newProfileData :: PlayerNumber -> PlayerSettings -> Maybe GameName -> Maybe SubmitRule -> Update ProfileDataState ProfileData
newProfileData uid ps gn sr =
    do pds@(ProfileDataState {..}) <- get
       case IxSet.getOne (profilesData @= uid) of
         Nothing -> do let profileData = ProfileData uid ps gn sr
                       put $ pds { profilesData = IxSet.updateIx uid profileData profilesData }
                       return profileData
         (Just profileData) -> return profileData

-- |
askProfileDataNumber :: Query ProfileDataState Int
askProfileDataNumber =
    do pds <- ask
       return $ IxSet.size $ profilesData pds

-- | get all 'ProfileData'
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

data Session = Session { _sh :: ServerHandle,
                         _multi :: Multi,
                         _acid :: Acid}

defaultMulti :: Settings -> UTCTime -> Multi
defaultMulti set t = Multi [] set t



type NomyxForm a = Form (ServerPartT IO) [Input] String Html () a

defaultPlayerSettings :: PlayerSettings
defaultPlayerSettings = PlayerSettings "" "" False False False False

instance FormError String where
    type ErrorInputType String = [Input]
    commonFormError _ = "common error"

$( makeLenses [''Multi, ''Settings, ''Network, ''PlayerSettings, ''Session, ''ProfileData] )


