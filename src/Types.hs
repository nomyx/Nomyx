
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, TypeFamilies,
             NamedFieldPuns, TemplateHaskell, FlexibleContexts #-}

module Types where
import Language.Nomyx.Expression
import Data.Typeable
import Data.List
import Data.Function
import Language.Haskell.Interpreter.Server (ServerHandle)
import Text.Blaze.Html5 hiding (map, label)
import Text.Reform
import Happstack.Server
import Text.Reform.Happstack()
import Network.BSD
import Data.Time
import Data.Lens.Template



type PlayerPassword = String
type Port = Int
data Network = Network {_host :: HostName, _port :: Port}
defaultNetwork = Network "" 0

data PlayerMulti = PlayerMulti   { _mPlayerNumber :: PlayerNumber,
                                       _mPlayerName :: PlayerName,
                                       _mPassword :: PlayerPassword,
                                       _mMail :: MailSettings,
                                       _inGame :: Maybe GameName,
                                       _lastRule :: Maybe SubmitRule}
                                       deriving (Eq, Show, Read, Typeable)

data Settings = Settings {_logs ::  Log,
                             _sh :: ServerHandle,
                             _net :: Network,
                             _sendMails :: Bool}

--- | A structure to hold the active games and players
data Multi = Multi {  _games   :: [Game],
                        _mPlayers :: [PlayerMulti],
                        _mSettings :: Settings,
                        _mCurrentTime :: UTCTime}
                        deriving (Typeable)


instance Show Multi where
   show Multi{_games, _mPlayers} = "Games: " ++ show (sort _games) ++ "\n" ++
                                 "Players: " ++ show (sort _mPlayers) ++ "\n" ++
                                 "Settings: " ++ (show _mSettings) ++ "\n" ++
                                 "current Time: " ++ (show _mCurrentTime) ++ "\n"


defaultMulti :: Settings -> UTCTime -> Multi
defaultMulti set t = Multi [] [] set t

data Log = Log { _logEvents :: [TimedEvent],
                  _logFilePath :: FilePath } deriving (Eq)

defaultLog :: FilePath ->Log
defaultLog fp = Log [] fp

data SubmitRule = SubmitRule RuleName String RuleCode deriving (Show, Read, Eq)

data TimedEvent = TE UTCTime MultiEvent deriving (Show, Read, Eq)

data MultiEvent =  MultiNewPlayer        PlayerMulti
                 | MultiNewGame            GameName GameDesc PlayerNumber
                 | MultiJoinGame           GameName PlayerNumber
                 | MultiLeaveGame          PlayerNumber
                 | MultiSubscribeGame     GameName PlayerNumber
                 | MultiUnsubscribeGame   GameName PlayerNumber
                 | MultiSubmitRule         SubmitRule PlayerNumber
                 | MultiInputChoiceResult EventNumber Int PlayerNumber
                 | MultiInputStringResult String String PlayerNumber
                 | MultiInputUpload        PlayerNumber FilePath String
                 | MultiTimeEvent          UTCTime
                 | MultiMailSettings      MailSettings PlayerNumber
                   deriving (Show, Read, Eq)


instance Ord PlayerMulti where
  (<=) = (<=) `on` _mPlayerNumber

type NomyxForm a = Form (ServerPartT IO) [Input] String Html () a


data MailSettings =
   MailSettings { _mailTo :: String,
                   _mailNewInput :: Bool,
                   _mailNewRule :: Bool,
                   _mailNewOutput :: Bool,
                   _mailConfirmed :: Bool } deriving (Eq, Show, Read)

defaultMailSettings :: MailSettings
defaultMailSettings = MailSettings "" False False False False

instance FormError String where
    type ErrorInputType String = [Input]
    commonFormError _ = "common error"

$( makeLenses [''Multi, ''PlayerMulti, ''Settings, ''Log, ''Network, ''MailSettings] )


