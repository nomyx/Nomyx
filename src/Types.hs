
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, TypeFamilies,
             NamedFieldPuns #-}

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


type PlayerPassword = String
type Port = Int
data Network = Network {host :: HostName, port :: Port}
defaultNetwork = Network "" 0

data PlayerMulti = PlayerMulti   { mPlayerNumber :: PlayerNumber,
                                   mPlayerName :: PlayerName,
                                   mPassword :: PlayerPassword,
                                   mMail :: MailSettings,
                                   inGame :: Maybe GameName,
                                   lastRule :: Maybe SubmitRule}
                                   deriving (Eq, Show, Read, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { games   :: [Game],
                     mPlayers :: [PlayerMulti],
                     logs ::  Log,
                     sh :: ServerHandle,
                     net :: Network,
                     mCurrentTime :: UTCTime}
                     deriving (Typeable)

instance Show Multi where
   show Multi{games=gs, mPlayers=mps} = show (sort gs) ++ "\n" ++ show (sort mps)

defaultMulti :: ServerHandle -> FilePath -> Network -> UTCTime -> Multi
defaultMulti sh fp net t = Multi [] [] (defaultLog fp) sh net t

data Log = Log { logEvents :: [TimedEvent],
                 logFilePath :: FilePath } deriving (Eq)

defaultLog :: FilePath ->Log
defaultLog fp = Log [] fp

data SubmitRule = SubmitRule RuleName String RuleCode deriving (Show, Read, Eq)

data TimedEvent = TE UTCTime MultiEvent deriving (Show, Read, Eq)

data MultiEvent =  MultiNewPlayer         PlayerMulti
                 | MultiNewGame           GameName String PlayerNumber
                 | MultiJoinGame          GameName PlayerNumber
                 | MultiLeaveGame         PlayerNumber
                 | MultiSubscribeGame     GameName PlayerNumber
                 | MultiUnsubscribeGame   GameName PlayerNumber
                 | MultiSubmitRule        SubmitRule PlayerNumber
                 | MultiInputChoiceResult EventNumber Int PlayerNumber
                 | MultiInputStringResult String String PlayerNumber
                 | MultiInputUpload       PlayerNumber FilePath String
                 | MultiTimeEvent         UTCTime
                 | MultiMailSettings      MailSettings PlayerNumber
                   deriving (Show, Read, Eq)


instance Ord PlayerMulti where
  (<=) = (<=) `on` mPlayerNumber

type NomyxForm a = Form (ServerPartT IO) [Input] String Html () a


data MailSettings = MailSettings { mailTo :: String,
                   mailNewInput :: Bool,
                   mailNewRule :: Bool,
                   mailNewOutput :: Bool,
                   mailConfirmed :: Bool } deriving (Eq, Show, Read)

defaultMailSettings :: MailSettings
defaultMailSettings = MailSettings "" False False False False

instance FormError String where
    type ErrorInputType String = [Input]
    commonFormError _ = "common error"

