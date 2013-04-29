
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, TypeFamilies,
             NamedFieldPuns, TemplateHaskell, FlexibleContexts #-}

module Types where
import Language.Nomyx
import Data.Typeable
import Data.Function
import Text.Blaze.Html5 hiding (map, label, base)
import Text.Reform
import Happstack.Server
import Text.Reform.Happstack()
import Network.BSD
import Data.Lens.Template
import Language.Nomyx.Game
import Language.Haskell.Interpreter.Server (ServerHandle)


type PlayerPassword = String
type Port = Int
data Network = Network {_host :: HostName, _port :: Port}
               deriving (Eq, Show, Read, Typeable)
defaultNetwork = Network "" 0

data PlayerMulti = PlayerMulti { _mPlayerNumber :: PlayerNumber,
                                 _mPlayerName   :: PlayerName,
                                 _mPassword     :: PlayerPassword,
                                 _mMail         :: MailSettings,
                                 _viewingGame   :: Maybe GameName,
                                 _lastRule      :: Maybe SubmitRule}
                                 deriving (Eq, Show, Read, Typeable)

data Settings = Settings { _logFilePath :: FilePath,
                           _net :: Network,
                           _sendMails :: Bool}
                           deriving (Eq, Show, Read, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { _games   :: [LoggedGame],
                     _mPlayers :: [PlayerMulti],
                     _mSettings :: Settings,
                     _mCurrentTime :: UTCTime}
                     deriving (Eq, Read, Show, Typeable)

data Session = Session { _sh :: ServerHandle, _multi :: Multi}


--instance Show Multi where
--   show Multi{_games, _mPlayers} = "Games: " ++ show (sort _games) ++ "\n" ++
--                                   "Players: " ++ show (sort _mPlayers) ++ "\n" ++
--                                   "Settings: " ++ (show _mSettings) ++ "\n" ++
--                                   "current Time: " ++ (show _mCurrentTime) ++ "\n"

defaultMulti :: Settings -> UTCTime -> Multi
defaultMulti set t = Multi [] [] set t --initialAuthState initialProfileState


instance Ord PlayerMulti where
  (<=) = (<=) `on` _mPlayerNumber

type NomyxForm a = Form (ServerPartT IO) [Input] String Html () a


data MailSettings =
   MailSettings { _mailTo :: String,
                  _mailNewInput :: Bool,
                  _mailNewRule :: Bool,
                  _mailNewOutput :: Bool,
                  _mailConfirmed :: Bool }
                  deriving (Eq, Show, Read)

defaultMailSettings :: MailSettings
defaultMailSettings = MailSettings "" False False False False

instance FormError String where
    type ErrorInputType String = [Input]
    commonFormError _ = "common error"

$( makeLenses [''Multi, ''PlayerMulti, ''Settings, ''Network, ''MailSettings, ''Session] )

