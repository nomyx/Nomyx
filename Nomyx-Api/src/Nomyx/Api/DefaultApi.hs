{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nomyx.Api.DefaultApi
     where

import GHC.Generics
import Data.Proxy
import Servant.API
import Servant.Client
import Servant
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Servant.Common.Text
import Data.List (intercalate)
import qualified Data.Text as T
import Nomyx.Api.Utils
import Test.QuickCheck
import Nomyx.Api.Model.Player
import Nomyx.Api.Model.Error
import Nomyx.Api.Model.NewPlayer
import Nomyx.Core.Session
import Nomyx.Core.Types
import Nomyx.Core.Profile
import           Control.Concurrent.STM
import           Control.Monad.State
import Control.Monad.Trans.Either
import Data.Swagger
import Language.Nomyx.Expression

type DefaultApi = "players" :> Get '[JSON] [ProfileData] -- playersGet
   -- :<|> "players" :> Capture "id" Integer :> Delete '[JSON] () -- playersIdDelete
   -- :<|> "players" :> Capture "id" Integer :> Get '[JSON] Player -- playersIdGet
   :<|> "players" :> ReqBody '[JSON] PlayerSettings :> Post '[JSON] ProfileData -- playersPost

proxyDefaultApi :: Proxy DefaultApi
proxyDefaultApi = Proxy

serverPath :: String
serverPath = "https://api.nomyx.net/v1"

parseHostPort :: String -> (String, Int)
parseHostPort path = (host,port)
    where
        authority = case parseURI path of
            Just x -> uriAuthority x
            _      -> Nothing
        (host, port) = case authority of
            Just y -> (uriRegName y, (getPort . uriPort) y)
            _      -> ("localhost", 8080)
        getPort p = case (length p) of
            0 -> 80
            _ -> (read . drop 1) p

(host, port) = parseHostPort serverPath

--newPlayer :: PlayerNumber -> PlayerSettings -> StateT Session IO ()

server :: TVar Session -> Server DefaultApi
server tv = (playersGet tv) :<|> (playersPost tv)

playersGet :: TVar Session -> EitherT ServantErr IO [ProfileData]
playersGet tv = do
   s <- liftIO $ atomically $ readTVar tv
   pds <- liftIO $ getAllProfiles s
   return pds

playersPost :: TVar Session -> PlayerSettings -> EitherT ServantErr IO ProfileData
playersPost tv ps = do
   s <- liftIO $ atomically $ readTVar tv
   pds <- liftIO $ getAllProfiles s
   return $ head pds

instance ToSchema ProfileData
instance ToSchema PlayerSettings
instance ToSchema RuleTemplate
instance ToSchema LastUpload
instance ToSchema Module
