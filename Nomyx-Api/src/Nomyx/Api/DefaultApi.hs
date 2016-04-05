{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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


type DefaultApi = "players" :> Get '[JSON] [Player] -- playersGet
   -- :<|> "players" :> Capture "id" Integer :> Delete '[JSON] () -- playersIdDelete
   -- :<|> "players" :> Capture "id" Integer :> Get '[JSON] Player -- playersIdGet
   -- :<|> "players" :> ReqBody '[JSON] NewPlayer :> Post '[JSON] Player -- playersPost

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

server :: Server DefaultApi
server = return []
--  :<|> return ()
--  :<|> Player "" 0
--  :<|> Player "" 1

--playersGet
--playersIdDelete
--    :<|> playersIdGet
--    :<|> playersPost
--    = client proxyDefaultApi $ BaseUrl Http host port
