
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Imprevu.Happstack.Test where

import Imprevu.Happstack.Forms
import Imprevu.Happstack.Types
import Imprevu.Test
import           Web.Routes.Happstack
import           Web.Routes.PathInfo
import           Web.Routes.RouteT                     (runRouteT)
import           Web.Routes.Site
import           Happstack.Server                      as HS
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Control.Monad.State

type TestServer a = RoutedServer TestIO TestState a
type TestWebState = WebState TestIO TestState

routedCommands :: Command -> TestServer Response
routedCommands Main             = nomyxPage
routedCommands (DoInput en fa ft) = newInput en fa ft

nomyxPage :: TestServer Response
nomyxPage = do
   (WebState tts _ _ _) <- get
   (TestState eis _ _) <- liftIO $ atomically $ readTVar tts
   m <- mapM viewInput eis
   return $ toResponse $ head $ catMaybes m

nomyxSite :: TestWebState -> Site Command (ServerPartT IO Response)
nomyxSite ws = setDefault Main $ mkSitePI $ (\a b -> evalStateT (runRouteT routedCommands a b) ws)


start :: IO ()
start = do
  let ts = TestState {eventInfos = [],
                      outputs    = [],
                      variable   = Var "" ""}
  let ts' = execEvents' testSingleInput ts
  tv <- atomically $ newTVar ts'
  launchWebServer tv

launchWebServer :: TVar TestState -> IO ()
launchWebServer ts = do
   putStrLn $ "Starting web server..."
   s <- liftIO $ atomically $ readTVar ts
   let conf = nullConf {HS.port = 8080}
   let ws = WebState {session = ts,
                      updateSession = error "updateSession",
                      evalFunc   = void . evalEvents,
                      errorHandler = undefined
                      }
   simpleHTTP conf $ server ws

--serving Nomyx web page as well as data from this package and the language library package
server :: TestWebState -> ServerPartT IO Response
server ws = mconcat [
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite ("http://127.0.0.1") "/Test" (nomyxSite ws)
       return $ toResponse html]

