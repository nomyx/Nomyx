{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Nomyx.Core.Serialize where

import           Control.Category
import           Control.Lens                        hiding ((.=))
import           Control.Monad.State
import           Data.Yaml                           (decodeEither, encode)
import qualified Data.ByteString.Char8            as BL
import           Language.Haskell.Interpreter.Server
import           Language.Nomyx                      hiding (getCurrentTime)
import           Nomyx.Core.Engine
import           Nomyx.Core.Interpret
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           Prelude                             hiding (log, (.))
import           System.FilePath                     ((</>))

save :: Multi -> IO ()
save m = BL.writeFile (getSaveFile $ _mSettings m) (encode m)

save' :: StateT Multi IO ()
save' = get >>= lift . save

load :: FilePath -> IO Multi
load fp = do
   s <- BL.readFile fp
   case decodeEither s of
      Left e -> error $ "error decoding save file: " ++ e
      Right a -> return a

loadMulti :: Settings -> ServerHandle -> IO Multi
loadMulti s sh = do
   let sd = getSaveFile s
   m <- load sd
   gs' <- mapM (updateGameInfo $ interRule sh) $ _gameInfos m
   let m' = set gameInfos gs' m
   let m'' = set mSettings s m'
   return m''

updateGameInfo :: InterpretRule -> GameInfo -> IO GameInfo
updateGameInfo f gi = do
   gi' <- updateLoggedGame f (_loggedGame gi)
   return $ gi {_loggedGame = gi'}

updateLoggedGame :: InterpretRule -> LoggedGame -> IO LoggedGame
updateLoggedGame f (LoggedGame g log) = getLoggedGame g f log

