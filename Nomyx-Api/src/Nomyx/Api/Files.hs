{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nomyx.Api.Files where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse
import Servant
import Servant.Server.Internal

-- Backends for file upload: in memory or in /tmp ?

data Mem
data Tmp

class KnownBackend b where
  type Storage b :: *

  withBackend :: Proxy b -> (BackEnd (Storage b) -> IO r) -> IO r

instance KnownBackend Mem where
  type Storage Mem = ByteString

  withBackend Proxy f = f lbsBackEnd

instance KnownBackend Tmp where
  type Storage Tmp = FilePath

  withBackend Proxy f = runResourceT . withInternalState $ \s ->
    f (tempFileBackEnd s)

-- * Files combinator, to get all of the uploaded files

data Files b

type MultiPartData b = ([Param], [File (Storage b)])

instance (KnownBackend b, HasServer api) => HasServer (Files b :> api) where
  type ServerT (Files b :> api) m =
    MultiPartData b -> ServerT api m

  route Proxy subserver req respond = withBackend pb $ \b -> do
    dat <- parseRequestBody b req
    route (Proxy :: Proxy api) (subserver dat) req respond

    where pb = Proxy :: Proxy b

type FilesMem = Files Mem
type FilesTmp = Files Tmp

