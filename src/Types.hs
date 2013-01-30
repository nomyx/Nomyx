
{-# LANGUAGE DeriveDataTypeable #-}

module Types where
import Game
import Language.Nomyx.Expression
import Data.Typeable
import Data.List
import Data.Function
import Language.Haskell.Interpreter.Server

type PlayerPassword = String


data PlayerMulti = PlayerMulti   { mPlayerNumber :: PlayerNumber,
                                   mPlayerName :: PlayerName,
                                   mPassword :: PlayerPassword,
                                   mMail :: String,
                                   inGame :: Maybe GameName}
                                   deriving (Eq, Show, Read, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { games   :: [Game],
                     mPlayers :: [PlayerMulti],
                     logs ::  Log,
                     sh :: ServerHandle}
                     deriving (Typeable)

data Log = Log { logEvents :: [MultiEvent],
                 logFilePath :: FilePath } deriving (Eq)

data MultiEvent =  MultiNewPlayer PlayerMulti
               | MultiNewGame String PlayerNumber
               | MultiJoinGame GameName PlayerNumber
               | MultiLeaveGame PlayerNumber
               | MultiSubscribeGame GameName PlayerNumber
               | MultiUnsubscribeGame GameName PlayerNumber
               | MultiSubmitRule String String String PlayerNumber
               | MultiInputChoiceResult EventNumber Int PlayerNumber
               | MultiInputStringResult String String PlayerNumber
               | MultiInputUpload PlayerNumber FilePath String deriving (Show, Read, Eq)

instance Show Multi where
   show Multi{games=gs, mPlayers=mps} = show (sort gs) ++ "\n" ++ show (sort mps)

instance Ord PlayerMulti where
  (<=) = (<=) `on` mPlayerNumber

defaultMulti :: ServerHandle -> FilePath -> Multi
defaultMulti sh fp = Multi [] [] (defaultLog fp) sh

defaultLog :: FilePath ->Log
defaultLog fp = Log [] fp


