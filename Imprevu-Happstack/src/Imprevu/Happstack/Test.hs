
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Imprevu.Happstack.Test where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Happstack.Server            as HS
import Imprevu.Happstack.Forms
import Imprevu.Happstack.Types
import Imprevu.Evaluation.InputEval
import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.TimeEval
import Imprevu.Test.Test
import Imprevu.Test.TestMgt
import Text.Blaze.Html5            (toHtml)
import Web.Routes.Happstack
import Web.Routes.PathInfo
import Web.Routes.RouteT           (runRouteT)
import Web.Routes.Site

type TestServer a = RoutedServer TestM TestState a
type TestWebState = WebState TestM TestState


start :: IO ()
start = do
  let ts = execSignals (testSingleInput >> testMultipleInputs) [] defaultEvalEnv
  tv <- atomically $ newTVar ts
  launchWebServer tv

launchWebServer :: TVar TestState -> IO ()
launchWebServer tv = do
   putStrLn $ "Starting web server..."
   let conf = nullConf {HS.port = 8080}
   let ws = WebState tv updateSessionTest (void . evalEvents) undefined
   forkIO $ launchTimeEvents tv (EvalFunc (void . evalEvents) undefined)
   simpleHTTP conf $ server ws

--serving Nomyx web page as well as data from this package and the language library package
server :: TestWebState -> ServerPartT IO Response
server ws = mconcat [
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite ("http://127.0.0.1") "/Test" (site ws)
       return $ toResponse html]

updateSessionTest :: TVar TestState -> InputResult -> IO ()
updateSessionTest tvs (InputResult en sa ff ida) = do
   s <- atomically $ readTVar tvs
   putStrLn $ show s
   putStrLn  $ "input result: EventNumber " ++ show en ++ ", SignalAddress " ++ show sa ++ ", Form " ++ show ff ++ ", choice " ++ show ida
   let ev = runEvalError' $ triggerInput ff ida sa en
   let (EvalEnv s' _ _) = execState ev (EvalEnv s (void . evalEvents) undefined)
   atomically $ writeTVar tvs s'

routedCommands :: Command -> TestServer Response
routedCommands Main               = mainPage
routedCommands (DoInput en fa ft) = newInput en fa ft

mainPage :: TestServer Response
mainPage = do
   (WebState tts _ _ _) <- get
   (TestState eis os _) <- liftIO $ atomically $ readTVar tts
   m <- mapM viewInput eis
   return $ toResponse $ do
     sequence_ $ catMaybes m
     toHtml $ show os

site :: TestWebState -> Site Command (ServerPartT IO Response)
site ws = setDefault Main $ mkSitePI $ (\a b -> evalStateT (runRouteT routedCommands a b) ws)
