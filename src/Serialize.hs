{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Serialize where

import Prelude hiding (log)
import Language.Nomyx hiding (getCurrentTime)
import Language.Nomyx.Engine
import Language.Nomyx.Engine.GameEvents
import Control.Monad.State
import Types
import Data.Lens
import Language.Haskell.Interpreter.Server
import Interpret
import Utils
import Data.Aeson.TH
import Data.Aeson
import Control.Applicative
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as BL

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
   gs' <- mapM (updateLoggedGame $ getRuleFunc sh) $ _games m
   let m' = games `setL` gs' $ m
   let m'' = mSettings `setL` set $ m'
   return m''

updateLoggedGame :: (RuleCode -> IO RuleFunc) -> LoggedGame -> IO LoggedGame
updateLoggedGame f (LoggedGame g log) = getLoggedGame g f log

time0 = posixSecondsToUTCTime 0

instance ToJSON Game where
   toJSON (Game name desc _ _ _ _ _ _ _ _ simu) =
      object ["gameName" .= name,
              "gameDesc" .= desc,
              "simu"     .= simu]

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
                            pure time0 <*>
                            v .: "simu"
   -- A non-Object value is of the wrong type, so fail.
   parseJSON _          = mzero


$(deriveJSON defaultOptions ''Multi)
$(deriveJSON defaultOptions ''Settings)
$(deriveJSON defaultOptions ''Network)
$(deriveJSON defaultOptions ''LoggedGame)
$(deriveJSON defaultOptions ''TimedEvent)
$(deriveJSON defaultOptions ''GameEvent)
$(deriveJSON defaultOptions ''UInputData)
$(deriveJSON defaultOptions ''SubmitRule)
$(deriveJSON defaultOptions ''GameDesc)
$(deriveJSON defaultOptions ''Simulation)

