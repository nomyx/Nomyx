{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Nomyx.Core.Serialize where

import Prelude hiding (log, (.))
import Language.Haskell.Interpreter.Server
import Data.Lens
import Data.Aeson.TH
import Data.Aeson
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.State
import Control.Applicative
import Control.Category
import Language.Nomyx hiding (getCurrentTime)
import Nomyx.Core.Types
import Nomyx.Core.Utils
import Nomyx.Core.Interpret
import Nomyx.Core.Engine
import Nomyx.Core.Engine.GameEvents

save :: Multi -> IO ()
save m = BL.writeFile (getSaveFile $ _mSettings m) (encode m)

save' :: StateT Multi IO ()
save' = get >>= lift . save

load :: FilePath -> IO Multi
load fp = do
   s <- BL.readFile fp
   case eitherDecode s of
      Left e -> error $ "error decoding save file: " ++ e
      Right a -> return a

loadMulti :: Settings -> ServerHandle -> IO Multi
loadMulti set sh = do
   m <- load (getSaveFile set)
   gs' <- mapM (updateGameInfo $ getRuleFunc sh) $ _gameInfos m
   let m' = gameInfos `setL` gs' $ m
   let m'' = mSettings `setL` set $ m'
   return m''

updateGameInfo :: (RuleCode -> IO Rule) -> GameInfo -> IO GameInfo
updateGameInfo f gi = do
   gi' <- updateLoggedGame f (_loggedGame gi)
   return $ gi {_loggedGame = gi'}

updateLoggedGame :: (RuleCode -> IO Rule) -> LoggedGame -> IO LoggedGame
updateLoggedGame f (LoggedGame g log) = getLoggedGame g f log

time0 = posixSecondsToUTCTime 0

instance ToJSON Game where
   toJSON (Game name desc _ _ _ _ _ _ _ _) =
      object ["gameName" .= name,
              "gameDesc" .= desc]

instance FromJSON Game where
   parseJSON (Object v) = Game <$>
      v .: "gameName" <*>
      v .: "gameDesc" <*>
      pure [] <*>
      pure [] <*>
      pure [] <*>
      pure [] <*>
      pure [] <*>
      pure Nothing <*>
      pure [] <*>
      pure time0
   -- A non-Object value is of the wrong type, so fail.
   parseJSON _ = mzero


$(deriveJSON defaultOptions ''Multi)
$(deriveJSON defaultOptions ''Settings)
$(deriveJSON defaultOptions ''Network)
$(deriveJSON defaultOptions ''LoggedGame)
$(deriveJSON defaultOptions ''GameInfo)
$(deriveJSON defaultOptions ''TimedEvent)
$(deriveJSON defaultOptions ''FieldAddressElem)
$(deriveJSON defaultOptions ''GameEvent)
$(deriveJSON defaultOptions ''InputData)
$(deriveJSON defaultOptions ''SubmitRule)
$(deriveJSON defaultOptions ''GameDesc)

