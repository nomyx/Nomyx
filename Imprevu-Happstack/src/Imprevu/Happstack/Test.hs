
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Imprevu.Happstack.Test where

import Imprevu.Happstack.Forms
import Imprevu.Happstack.Types
import Imprevu.Test
import Imprevu.Inputs
import Imprevu.Internal.InputEval
import Imprevu.Internal.EventEval
import           Web.Routes.Happstack
import           Web.Routes.PathInfo
import           Web.Routes.RouteT                     (runRouteT)
import           Web.Routes.Site
import           Happstack.Server                      as HS
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Control.Monad.State
import           Text.Blaze.Html5            (toHtml)

type TestServer a = RoutedServer TestIO TestState a
type TestWebState = WebState TestIO TestState

routedCommands :: Command -> TestServer Response
routedCommands Main             = nomyxPage
routedCommands (DoInput en fa ft) = newInput en fa ft

nomyxPage :: TestServer Response
nomyxPage = do
   (WebState tts _ _ _) <- get
   (TestState eis os _) <- liftIO $ atomically $ readTVar tts
   m <- mapM viewInput eis
   return $ toResponse $ do
     head $ catMaybes m
     toHtml $ show os

nomyxSite :: TestWebState -> Site Command (ServerPartT IO Response)
nomyxSite ws = setDefault Main $ mkSitePI $ (\a b -> evalStateT (runRouteT routedCommands a b) ws)


start :: IO ()
start = do
  let ts = TestState {eventInfos = [],
                      outputs    = [],
                      variable   = Var "" ""}
  let ts' = execEvents' testSingleInput' ts
  tv <- atomically $ newTVar ts'
  launchWebServer tv

launchWebServer :: TVar TestState -> IO ()
launchWebServer ts = do
   putStrLn $ "Starting web server..."
   let conf = nullConf {HS.port = 8080}
   let ws = WebState ts updateSessionTest (void . evalEvents) undefined
   simpleHTTP conf $ server ws

--serving Nomyx web page as well as data from this package and the language library package
server :: TestWebState -> ServerPartT IO Response
server ws = mconcat [
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite ("http://127.0.0.1") "/Test" (nomyxSite ws)
       return $ toResponse html]

-- Test input
testSingleInput' :: TestIO ()
testSingleInput' = void $ onInputRadio_ "Vote for Holland or Sarkozy" [Holland, Sarkozy] h 1 where
   h a = putStrLn' ("voted for " ++ show a)

--testMultipleInputs :: TestIO ()
--testMultipleInputs = void $ onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
--   h a = do
--     liftIO $ putStrLn "callback"
--     putStrLn' ("voted for " ++ show a)

updateSessionTest :: TVar TestState -> InputResult -> IO ()
updateSessionTest tvs (InputResult en sa ff ida) = do
   s <- atomically $ readTVar tvs
   putStrLn $ show s
   putStrLn  $ "input result: EventNumber " ++ show en ++ ", SignalAddress " ++ show sa ++ ", Form " ++ show ff ++ ", choice " ++ show ida
   let ev = runEvalError' $ triggerInput (SInputView ff) (SInputDataView ida) sa en
   let (EvalEnv s' _ _) = execState ev (EvalEnv s (void . evalEvents) undefined)
   atomically $ writeTVar tvs s'

