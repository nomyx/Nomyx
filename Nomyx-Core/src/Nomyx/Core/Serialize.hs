{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Nomyx.Core.Serialize where

import           Prelude                             hiding (log, (.))
import           Control.Category
import           Control.Lens                        hiding ((.=))
import           Control.Monad.State
import           Data.Yaml                           (decodeEither, encode)
import           Data.List
import qualified Data.Text.IO                        as DT
import qualified Data.ByteString.Char8               as BL
import           Nomyx.Core.Engine
import           Nomyx.Core.Engine.Interpret
import           Nomyx.Core.Types
import           Nomyx.Core.Utils
import           Nomyx.Language
import           System.FilePath

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

loadMulti :: Settings -> IO Multi
loadMulti s = do
   let sd = getSaveFile s
   m <- load sd
   gs' <- mapM updateGameInfo (_gameInfos m)
   let m' = set gameInfos gs' m
   let m'' = set mSettings s m'
   return m''

updateGameInfo :: GameInfo -> IO GameInfo
updateGameInfo gi = do
   gi' <- updateLoggedGame (_loggedGame gi)
   return $ gi {_loggedGame = gi'}

updateLoggedGame :: LoggedGame -> IO LoggedGame
updateLoggedGame (LoggedGame g log) = getLoggedGame g log

-- read a library file
readLibrary :: FilePath -> IO Library
readLibrary yamlFile = do
   let dir = takeDirectory yamlFile
   ts <- readTemplates yamlFile
   let mods = nub $ join [ms | (RuleTemplate _ _ _ _ _ _ ms) <- ts]
   ms <- mapM (readModule dir) mods
   return $ Library ts ms

readTemplates :: FilePath -> IO [RuleTemplate]
readTemplates yamlFile = do
  s <- BL.readFile yamlFile
  case decodeEither s of
     Left e -> error $ "error decoding library: " ++ e
     Right ts -> return ts

readModule :: FilePath -> FilePath -> IO ModuleInfo
readModule basePath mod = do
  let absPath = basePath </> mod
  content <- DT.readFile absPath
  return $ ModuleInfo mod content

-- write a library file
writeLibrary :: FilePath -> Library -> IO ()
writeLibrary yamlFile (Library ts ms) = do
   let dir = takeDirectory yamlFile
   putStrLn $ "saving templates: " ++ yamlFile
   BL.writeFile yamlFile (encode ts)
   mapM_ (saveModule dir) ms
   
