{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nomyx.Api.Api
     where

import           GHC.Generics
import           Data.Proxy
import           Data.Yaml
import           Servant.API
import           Servant.Client
import           Servant
import           Network.URI (URI (..), URIAuth (..), parseURI)
import           Data.Maybe (fromMaybe)
import           Data.List (intercalate)
import           Data.Maybe
import qualified Data.Text as T
import           Test.QuickCheck
import           Nomyx.Api.Model.Player
import           Nomyx.Api.Model.Error
import           Nomyx.Api.Model.NewPlayer
import           Nomyx.Core.Session hiding (getModules)
import           Nomyx.Core.Types
import           Nomyx.Core.Profile
import           Control.Concurrent.STM
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.Swagger
import           Data.Swagger.Schema
import           Nomyx.Language.Types
import           Control.Monad.Except
import           Network.Wai.Parse
import qualified Data.ByteString.Char8 as B
import           System.Log.Logger

-- * API definition

type NomyxApi = PlayerApi :<|> RuleTemplateApi

type PlayerApi =  "players" :>                                   Get  '[JSON] [ProfileData] -- playersGet
             :<|> "players" :> ReqBody '[JSON] PlayerSettings :> Post '[JSON] ProfileData -- playersPost
             :<|> "players" :> Capture "id" Int               :> Get '[JSON] ProfileData
             :<|> "players" :> Capture "id" Int               :> Delete '[JSON] ()


type RuleTemplateApi =  "templates" :>                                   Get  '[JSON] Library  --get all templates
                   :<|> "templates" :> ReqBody '[JSON] Library        :> Put  '[JSON] () -- replace all templates

nomyxApi :: Proxy NomyxApi
nomyxApi = Proxy

serverPath :: String
serverPath = "https://api.nomyx.net/v1"

parseHostPort :: String -> (String, Int)
parseHostPort path = (myhost,myport)
    where
        authority = case parseURI path of
            Just x -> uriAuthority x
            _      -> Nothing
        (myhost, myport) = case authority of
            Just y -> (uriRegName y, (getPort . uriPort) y)
            _      -> ("localhost", 8080)
        getPort p = case (length p) of
            0 -> 80
            _ -> (read . drop 1) p

(host, port) = parseHostPort serverPath

server :: TVar Session -> Server NomyxApi
server tv = ((playersGet tv)   :<|> (playersPost tv)   :<|> (playerGet tv) :<|> (playerDelete tv))
       :<|> ((templatesGet tv) :<|> (templatesPut tv))

-- * Players API

playersGet :: TVar Session -> ExceptT ServantErr IO [ProfileData]
playersGet tv = do
   s <- liftIO $ atomically $ readTVar tv
   pds <- liftIO $ getAllProfiles s
   return pds

playersPost :: TVar Session -> PlayerSettings -> ExceptT ServantErr IO ProfileData
playersPost tv ps = do
   liftIO $ updateSession tv (newPlayer 2 ps)
   s <- liftIO $ atomically $ readTVar tv
   pds <- liftIO $ getAllProfiles s
   return $ head pds

playerGet :: TVar Session -> PlayerNumber -> ExceptT ServantErr IO ProfileData
playerGet tv pn = do
   s <- liftIO $ atomically $ readTVar tv
   mpd <- liftIO $ getProfile s pn
   case mpd of
     Just pd -> return pd
     Nothing -> throwError $ err410 { errBody = "Player does not exist." }

playerDelete :: TVar Session -> PlayerNumber -> ExceptT ServantErr IO ()
playerDelete tv pn = error "not supported"

-- * Templates API

templatesGet :: TVar Session -> ExceptT ServantErr IO Library
templatesGet tv = do
   s <- liftIO $ atomically $ readTVar tv
   return $ _mLibrary $ _multi s

templatesPost :: TVar Session -> RuleTemplate -> ExceptT ServantErr IO ()
templatesPost tv rt = do
   liftIO $ updateSession tv (newRuleTemplate 1 rt)
   return ()

templatesPut :: TVar Session -> Library -> ExceptT ServantErr IO ()
templatesPut tv lib = liftIO $ do
   debug $ "templatesPut library: " ++ (show lib)
   updateSession tv (updateLibrary  1lib)
   return ()

debug, info :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "Nomyx.Api.Api" s
info s = liftIO $ infoM "Nomyx.Api.Api" s
