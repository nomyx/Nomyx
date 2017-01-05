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
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (fromMaybe)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Typeable
import qualified Data.Text as T
import           Data.Swagger
import           Data.Swagger.Schema
import           Servant.API
import           Servant.Client
import           Servant
import           Network.URI (URI (..), URIAuth (..), parseURI)
import           Network.Wai.Parse
import           Nomyx.Api.Model.Player
import           Nomyx.Api.Model.Error
import           Nomyx.Api.Model.NewPlayer
import           Nomyx.Core.Session hiding (getModules)
import           Nomyx.Core.Types
import           Nomyx.Core.Profile
import           Nomyx.Language.Types
import           Control.Concurrent.STM
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Except
import           System.Log.Logger
import           Test.QuickCheck

-- * API definition

type NomyxApi = PlayerApi :<|> RuleTemplateApi

type PlayerApi =  "players" :>                                   Get    '[JSON] [ProfileData] 
             :<|> "players" :> ReqBody '[JSON] PlayerSettings :> Post   '[JSON] ProfileData 
             :<|> "players" :> Capture "id" Int               :> Get    '[JSON] ProfileData
             :<|> "players" :> Capture "id" Int               :> Delete '[JSON] ()


type RuleTemplateApi =  "templates" :> BasicAuth "foo-realm" PlayerNumber :>                            Get '[JSON] Library  -- get all templates
                   :<|> "templates" :> BasicAuth "foo-realm" PlayerNumber :> ReqBody '[JSON] Library :> Put '[JSON] ()       -- replace all templates

nomyxApi :: Proxy NomyxApi
nomyxApi = Proxy

serverPath :: String
serverPath = "https://api.nomyx.net/v1"

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

templatesGet :: TVar Session -> PlayerNumber -> ExceptT ServantErr IO Library
templatesGet tv _ = do
   s <- liftIO $ atomically $ readTVar tv
   return $ _mLibrary $ _multi s

templatesPost :: TVar Session -> PlayerNumber -> RuleTemplate -> ExceptT ServantErr IO ()
templatesPost tv _ rt = do
   liftIO $ updateSession tv (newRuleTemplate 1 rt)
   return ()

templatesPut :: TVar Session -> PlayerNumber -> Library -> ExceptT ServantErr IO ()
templatesPut tv pn lib = liftIO $ do
   debug $ "templatesPut library: " ++ (show lib)
   updateSession tv (updateLibrary pn lib)
   return ()

debug, info :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "Nomyx.Api.Api" s
info s = liftIO $ infoM "Nomyx.Api.Api" s
